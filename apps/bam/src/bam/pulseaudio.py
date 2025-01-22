"""PulseAudio system interface."""

from __future__ import annotations

from enum import Enum

from .logging import logging

from .types import AudioMode
from .utils import run_command

logger = logging.getLogger(__name__)


class AudioProfile(str, Enum):
    """Audio profiles for Bluetooth devices."""

    A2DP_SINK = "a2dp-sink"
    HSP = "headset-head-unit"
    HFP = "handsfree-head-unit"


class DevicePrefix(str, Enum):
    """Device naming prefixes."""

    CARD = "bluez_card."
    SINK = "bluez_sink."


class PulseAudioController:
    """Interface to PulseAudio functionality via pactl."""

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
        profile = (
            AudioProfile.A2DP_SINK if mode == AudioMode.MUSIC else AudioProfile.HSP
        )
        mac_normalized = self._normalize_mac(mac_address)
        run_command(
            f"pactl set-card-profile {DevicePrefix.CARD.value}{mac_normalized} {profile.value}",
        )

    def detect_device_supported_modes(self, mac_address: str) -> list[AudioMode]:
        """Detect audio modes supported by a device."""
        modes = []
        if card_info := self._get_card_info(mac_address):
            if AudioProfile.A2DP_SINK in card_info.lower():
                modes.append(AudioMode.MUSIC)
            if any(
                profile in card_info.lower()
                for profile in [AudioProfile.HSP, AudioProfile.HFP]
            ):
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
                run_command(f"pactl set-default-sink {sink_name}")
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
        stdout, _ = run_command("pactl list cards")
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
        stdout, _ = run_command("pactl list short sinks")
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
