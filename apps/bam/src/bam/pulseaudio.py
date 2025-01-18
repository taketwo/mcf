"""PulseAudio system interface."""

from __future__ import annotations

import logging
import subprocess
from enum import Enum

from .types import AudioMode

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

    def get_card_profile(self, mac_address: str) -> AudioMode | None:
        """Get current audio mode of a device."""
        if device_card := self._get_card_info(mac_address):
            for line in device_card.splitlines():
                if "active profile:" in line.lower():
                    active_profile = line.split(":", 1)[1].strip().lower()
                    if active_profile.startswith(AudioProfile.A2DP_SINK):
                        return AudioMode.MUSIC
                    if active_profile.startswith((AudioProfile.HSP, AudioProfile.HFP)):
                        return AudioMode.CALL
                    break
        return None

    def set_card_profile(self, mac_address: str, mode: AudioMode) -> None:
        """Set the audio profile for a device."""
        profile = (
            AudioProfile.A2DP_SINK if mode == AudioMode.MUSIC else AudioProfile.HSP
        )
        mac_normalized = mac_address.replace(":", "_")
        self._run_command(
            f"set-card-profile {DevicePrefix.CARD.value}{mac_normalized} {profile.value}",
        )

    def detect_supported_modes(self, mac_address: str) -> list[AudioMode]:
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

    def _get_card_info(self, mac_address: str) -> str | None:
        """Get card info for a device from PulseAudio."""
        stdout, _ = self._run_command("list cards")
        cards = stdout.split("Card #")
        mac_normalized = mac_address.replace(":", "_")

        for card in cards:
            if mac_normalized in card:
                return card
        return None

    def _get_available_sinks(self) -> list[str]:
        """Get list of available PulseAudio sinks."""
        stdout, _ = self._run_command("list short sinks")
        return [
            parts[1]
            for parts in (
                line.split("\t") for line in stdout.splitlines() if line.strip()
            )
            if len(parts) >= 2
        ]

    def _run_command(self, command: str) -> tuple[str, str]:
        """Run pactl command and return its output."""
        full_command = f"pactl {command}"
        process = subprocess.Popen(
            full_command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            shell=True,
            text=True,
        )
        return process.communicate()

    def _get_available_sinks_for_device(self, mac_address: str) -> list[str]:
        """Get the PulseAudio sink name(s) for a Bluetooth device."""
        mac_normalized = mac_address.replace(":", "_")
        sinks = self._get_available_sinks()
        return [sink for sink in sinks if mac_normalized in sink]

    def set_as_default_sink(self, mac_address: str) -> None:
        """Make device the system default audio output."""
        if sinks := self._get_available_sinks_for_device(mac_address):
            if len(sinks) > 1:
                logger.warning(
                    "Multiple sinks found for device with MAC %s: %s",
                    mac_address,
                    ", ".join(sinks),
                )
            for sink_name in sinks:
                self._run_command(f"set-default-sink {sink_name}")
                return
        else:
            raise RuntimeError(
                f"No valid audio sink found for device with MAC {mac_address}",
            )
