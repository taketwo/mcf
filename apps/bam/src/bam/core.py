"""Core functionality for Bluetooth Audio Manager."""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Any, TYPE_CHECKING

import yaml
from systemd.journal import JournalHandler

from .bluetooth import BluetoothController
from .pulseaudio import PulseAudioController
from .types import AudioMode

if TYPE_CHECKING:
    from pathlib import Path

logger = logging.getLogger(__name__)
logger.addHandler(JournalHandler())


@dataclass
class BTDevice:
    """Represents a Bluetooth device with its capabilities and current state."""

    mac_address: str
    name: str
    aliases: list[str]
    supported_modes: list[AudioMode]
    default_mode: AudioMode

    def matches_identifier(self, identifier: str) -> bool:
        """Check if device matches given identifier (MAC, name, or alias)."""
        return identifier in (self.mac_address, self.name) or identifier in self.aliases


class BTManager:
    """Core logic for managing Bluetooth devices."""

    def __init__(
        self,
        config_path: Path,
        bluetooth: BluetoothController | None = None,
        pulseaudio: PulseAudioController | None = None,
    ) -> None:
        """Initialize BTManager with optional custom controllers."""
        self.config_path = config_path
        self.devices: dict[str, BTDevice] = {}
        self.bluetooth = bluetooth or BluetoothController()
        self.pulseaudio = pulseaudio or PulseAudioController()
        self.load_config()

    def load_config(self) -> None:
        """Load device configuration from YAML file."""
        if not self.config_path.exists():
            return

        with self.config_path.open() as f:
            config = yaml.safe_load(f) or {}

        for mac, info in config.get("devices", {}).items():
            self.devices[mac] = BTDevice(
                mac_address=mac,
                name=info["name"],
                aliases=info.get("aliases", []),
                supported_modes=[AudioMode[m.upper()] for m in info["supported_modes"]],
                default_mode=AudioMode[info["default_mode"].upper()],
            )

    def save_config(self) -> None:
        """Save current device configuration to YAML file."""
        config: dict = {"devices": {}}
        for mac, device in self.devices.items():
            config["devices"][mac] = {
                "name": device.name,
                "aliases": device.aliases,
                "supported_modes": [m.name.lower() for m in device.supported_modes],
                "default_mode": device.default_mode.name.lower(),
            }

        self.config_path.parent.mkdir(parents=True, exist_ok=True)
        with self.config_path.open("w") as f:
            yaml.dump(config, f)

    def find_device(self, identifier: str | None) -> BTDevice | None:
        """Look up device by MAC, name, or alias."""
        if not identifier:
            connected = self.bluetooth.get_connected_devices()
            known_connected = [
                dev
                for dev in self.devices.values()
                if any(conn.mac_address == dev.mac_address for conn in connected)
            ]
            return known_connected[0] if len(known_connected) == 1 else None

        return next(
            (
                dev
                for dev in self.devices.values()
                if dev.matches_identifier(identifier)
            ),
            None,
        )

    def connect_device(
        self,
        device: BTDevice,
        mode: AudioMode | None = None,
    ) -> None:
        """Connect to a device and set its mode."""
        mode = mode or device.default_mode
        logger.info("Connecting to %s...", device.name)

        self.bluetooth.connect_device(device.mac_address)
        self.pulseaudio.set_card_profile(device.mac_address, mode)
        # FIXME: This often fails on first connect attempt, probably because the sink
        # is not ready yet. We should either retry or wait for the sink to be available.
        self.pulseaudio.set_as_default_sink(device.mac_address)

    def disconnect_device(self, device: BTDevice) -> None:
        """Disconnect a device."""
        logger.info("Disconnecting %s...", device.name)
        self.bluetooth.disconnect_device(device.mac_address)

    def set_device_mode(self, device: BTDevice, mode: AudioMode) -> None:
        """Switch device to specified audio mode."""
        if mode not in device.supported_modes:
            raise ValueError(f"Mode {mode.name} not supported by device {device.name}")

        self.pulseaudio.set_card_profile(device.mac_address, mode)
        logger.info("Switched %s to %s mode", device.name, mode.name.lower())

    def toggle_mode(self, device: BTDevice) -> None:
        """Toggle device between music and call modes."""
        current_mode = self.pulseaudio.get_card_profile(device.mac_address)
        if current_mode is None:
            logger.error("Could not determine current mode for device %s", device.name)
            return

        new_mode = (
            AudioMode.CALL if current_mode == AudioMode.MUSIC else AudioMode.MUSIC
        )
        if new_mode not in device.supported_modes:
            logger.error(
                "Mode %s is not supported by device %s",
                new_mode.name.lower(),
                device.name,
            )
            return

        self.set_device_mode(device, new_mode)

    def add_device(
        self,
        mac_address: str,
        name: str,
        aliases: list[str],
        supported_modes: list[AudioMode],
        default_mode: AudioMode,
    ) -> None:
        """Add new device to known devices."""
        self.devices[mac_address] = BTDevice(
            mac_address=mac_address,
            name=name,
            aliases=aliases,
            supported_modes=supported_modes,
            default_mode=default_mode,
        )
        self.save_config()

    def get_device_status(self) -> dict[str, list[dict[str, Any]]]:
        """Get comprehensive status of all known and connected devices."""
        connected_devices = self.bluetooth.get_connected_devices()
        connected_macs = {d.mac_address for d in connected_devices}

        known_info = []
        for device in self.devices.values():
            info = {
                "name": device.name,
                "mac_address": device.mac_address,
                "aliases": device.aliases,
                "supported_modes": device.supported_modes,
                "default_mode": device.default_mode,
                "is_connected": device.mac_address in connected_macs,
            }
            if info["is_connected"]:
                info["current_mode"] = self.pulseaudio.get_card_profile(
                    device.mac_address,
                )
            known_info.append(info)

        connected_info = []
        for device in connected_devices:
            info = {
                "name": device.name,
                "mac_address": device.mac_address,
                "current_mode": self.pulseaudio.get_card_profile(device.mac_address),
                "battery_level": self.bluetooth.get_battery_level(device.mac_address),
                "is_known": device.mac_address in self.devices,
            }
            if info["is_known"]:
                known_device = self.devices[device.mac_address]
                info["aliases"] = known_device.aliases
            connected_info.append(info)

        return {
            "known": known_info,
            "connected": connected_info,
        }
