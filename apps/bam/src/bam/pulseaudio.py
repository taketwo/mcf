"""PulseAudio system interface."""

from __future__ import annotations

from enum import Enum

from .audio import AudioMode
from .logging import logging
from .utils import CommandError, run_command_check

logger = logging.getLogger(__name__)


class PulseAudioError(Exception):
    """PulseAudio (pactl) operation failed."""


class AudioProfile(str, Enum):
    """Audio profiles for Bluetooth devices."""

    A2DP_SINK = "a2dp-sink"
    HSP = "headset-head-unit"
    HFP = "handsfree-head-unit"


class DevicePrefix(str, Enum):
    """Device naming prefixes."""

    CARD = "bluez_card."
    SINK = "bluez_sink."


def _parse_profile_names_from_card(card: str) -> list[str]:
    """Extract profile identifiers from one `pactl list cards` card block."""
    names: list[str] = []
    in_profiles = False
    for line in card.splitlines():
        if not in_profiles:
            if line.strip().startswith("Profiles:"):
                in_profiles = True
            continue

        stripped = line.strip()
        if stripped.startswith("Active Profile:") or stripped.startswith("Ports:"):
            break
        if not stripped:
            continue
        if ":" not in stripped:
            continue
        name = stripped.split(":", 1)[0].strip()
        if name:
            names.append(name)
    return names


def _select_a2dp_sink_profile(profile_names: list[str]) -> str | None:
    """Pick the first A2DP sink profile (PipeWire uses codec suffixes, e.g. a2dp-sink-sbc)."""
    prefix = AudioProfile.A2DP_SINK.value
    for name in profile_names:
        if name.lower().startswith(prefix):
            return name
    return None


def _select_headset_profile(profile_names: list[str]) -> str | None:
    """Pick an HSP/HFP card profile, preferring mSBC over CVSD over generic."""
    hsp = AudioProfile.HSP.value
    hfp = AudioProfile.HFP.value
    headsets = [n for n in profile_names if n == hsp or n.startswith(f"{hsp}-")]
    if not headsets:
        hfps = [n for n in profile_names if n.startswith(hfp)]
        return hfps[0] if hfps else None
    for preferred in (f"{hsp}-msbc", f"{hsp}-cvsd", hsp):
        if preferred in headsets:
            return preferred
    return headsets[0]


class PulseAudioController:
    """Interface to PulseAudio functionality via pactl."""

    def _pactl(self, command: str) -> tuple[str, str]:
        try:
            return run_command_check(command)
        except CommandError as e:
            detail = e.stderr.strip() or e.stdout.strip() or "(no output)"
            raise PulseAudioError(
                f"PulseAudio command failed (exit {e.returncode}): {detail}",
            ) from e

    def get_device_mode(self, mac_address: str) -> AudioMode | None:
        """Get current audio mode of a device."""
        logger.debug("Getting audio mode for device with MAC %s", mac_address)
        if device_card := self._get_card_info(mac_address):
            for line in device_card.splitlines():
                if "active profile:" in line.lower():
                    active_profile = line.split(":", 1)[1].strip().lower()
                    logger.debug("Active profile: %s", active_profile)
                    if active_profile.startswith(AudioProfile.A2DP_SINK):
                        return AudioMode.MUSIC
                    if active_profile.startswith((AudioProfile.HSP, AudioProfile.HFP)):
                        return AudioMode.CALL
                    break
            logger.warning(
                "Active profile not found or not recognized for device with MAC %s",
                mac_address,
            )
        return None

    def set_device_mode(self, mac_address: str, mode: AudioMode) -> None:
        """Set audio mode for a device."""
        logger.debug(
            "Setting audio mode for device with MAC %s to %s",
            mac_address,
            mode,
        )
        mac_normalized = self._normalize_mac(mac_address)
        card = self._get_card_info(mac_address)
        if card is None:
            raise PulseAudioError(
                f"No PulseAudio card found for Bluetooth device {mac_address} "
                f"({DevicePrefix.CARD.value}{mac_normalized})",
            )

        profile_names = _parse_profile_names_from_card(card)
        if mode == AudioMode.MUSIC:
            profile = _select_a2dp_sink_profile(profile_names)
            if profile is None:
                raise PulseAudioError(
                    "No A2DP sink profile on PulseAudio card "
                    f"{DevicePrefix.CARD.value}{mac_normalized} "
                    f"(profiles: {', '.join(profile_names) or '(none)'}). "
                    "Reconnect the headset or wait for Bluetooth service discovery.",
                )
        else:
            profile = _select_headset_profile(profile_names)
            if profile is None:
                raise PulseAudioError(
                    "No headset (HSP/HFP) profile on PulseAudio card "
                    f"{DevicePrefix.CARD.value}{mac_normalized} "
                    f"(profiles: {', '.join(profile_names) or '(none)'}).",
                )

        logger.debug("Using card profile %s for %s mode", profile, mode.name.lower())
        self._pactl(
            f"pactl set-card-profile {DevicePrefix.CARD.value}{mac_normalized} {profile}",
        )

    def detect_device_supported_modes(self, mac_address: str) -> list[AudioMode]:
        """Detect audio modes supported by a device."""
        modes: list[AudioMode] = []
        if not (card_info := self._get_card_info(mac_address)):
            return modes

        profile_names = _parse_profile_names_from_card(card_info)
        if _select_a2dp_sink_profile(profile_names) is not None:
            modes.append(AudioMode.MUSIC)
        if _select_headset_profile(profile_names) is not None:
            modes.append(AudioMode.CALL)
        return modes

    def set_default_sink(self, mac_address: str) -> None:
        """Make device the system default audio output."""
        logger.debug(
            "Setting device with MAC %s as system default audio sink",
            mac_address,
        )
        if sinks := self._get_available_sinks(mac_address):
            if len(sinks) > 1:
                logger.warning(
                    "Multiple sinks found for device with MAC %s: %s",
                    mac_address,
                    ", ".join(sinks),
                )
            for sink_name in sinks:
                self._pactl(f"pactl set-default-sink {sink_name}")
                return
        else:
            raise RuntimeError(
                f"No valid audio sink found for device with MAC {mac_address}",
            )

    def _normalize_mac(self, mac_address: str) -> str:
        """Convert MAC address to PulseAudio format by replacing colons with underscores."""
        return mac_address.replace(":", "_")

    def _get_card_info(self, mac_address: str) -> str | None:
        """Get card info for a device from PulseAudio."""
        stdout, _ = self._pactl("pactl list cards")
        cards = stdout.split("Card #")
        mac_normalized = self._normalize_mac(mac_address)

        for card in cards:
            if mac_normalized in card:
                return card
        return None

    def _get_available_sinks(self, mac_address: str | None = None) -> list[str]:
        """Get list of available PulseAudio sinks.

        Parameters
        ----------
        mac_address : str, optional
            MAC address of the device to filter the sinks.

        Returns
        -------
        list[str]
            List of sink names.

        """
        if not mac_address:
            logger.debug("Getting available sinks")
        else:
            logger.debug("Getting available sinks for device with MAC %s", mac_address)
        stdout, _ = self._pactl("pactl list short sinks")
        sinks = [
            parts[1]
            for parts in (
                line.split("\t") for line in stdout.splitlines() if line.strip()
            )
            if len(parts) >= 2
        ]

        if mac_address:
            sinks = [sink for sink in sinks if self._normalize_mac(mac_address) in sink]

        logger.debug("Found sinks: %s", ", ".join(sinks))
        return sinks
