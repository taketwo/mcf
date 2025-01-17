"""Bluetooth system interface using bluetoothctl."""

from __future__ import annotations

import logging
import subprocess
from dataclasses import dataclass

logger = logging.getLogger(__name__)


@dataclass
class ConnectedDevice:
    """Information about a connected Bluetooth device."""

    mac_address: str
    name: str


class BluetoothController:
    """Interface to system Bluetooth functionality via bluetoothctl."""

    def _run_command(self, command: str) -> tuple[str, str]:
        """Run bluetoothctl command and return its output."""
        full_command = f"bluetoothctl {command}"
        process = subprocess.Popen(
            full_command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            shell=True,
            text=True,
        )
        return process.communicate()

    def get_connected_devices(self) -> list[ConnectedDevice]:
        """Get list of currently connected Bluetooth devices."""
        stdout, _ = self._run_command("devices Connected")
        devices = []
        for line in stdout.splitlines():
            if not line.strip() or not line.startswith("Device"):
                continue
            parts = line.split(None, 2)
            if len(parts) >= 3:
                devices.append(
                    ConnectedDevice(
                        mac_address=parts[1],
                        name=parts[2],
                    ),
                )
        return devices

    def get_battery_level(self, mac_address: str) -> int | None:
        """Get battery level for a device if available."""
        stdout, _ = self._run_command(f"info {mac_address}")

        for line in stdout.splitlines():
            if "Battery Percentage:" in line:
                try:
                    # Line format: "\tBattery Percentage: 0x64 (100)"
                    value = line.split("(")[1].split(")")[0]
                    return int(value)
                except (IndexError, ValueError):
                    return None
        return None

    def connect_device(self, mac_address: str) -> None:
        """Connect to a Bluetooth device."""
        self._run_command(f"connect {mac_address}")

    def disconnect_device(self, mac_address: str) -> None:
        """Disconnect a Bluetooth device."""
        self._run_command(f"disconnect {mac_address}")
