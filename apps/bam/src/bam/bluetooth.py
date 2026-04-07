"""Bluetooth system interface using bluetoothctl."""

from __future__ import annotations

import time
from dataclasses import dataclass

from .logging import get_logger
from .utils import CommandError, run_command_check

logger = get_logger(__name__)


class BluetoothError(Exception):
    """Bluetooth operation failed."""


@dataclass
class ConnectedDevice:
    """Information about a connected Bluetooth device."""

    mac_address: str
    name: str


class BluetoothController:
    """Interface to system Bluetooth functionality via bluetoothctl."""

    def _bluetoothctl(self, command: str, err_prefix: str) -> tuple[str, str]:
        try:
            return run_command_check(command)
        except CommandError as e:
            detail = e.stderr.strip() or e.stdout.strip()
            msg = f"{err_prefix}: {detail}" if detail else err_prefix
            raise BluetoothError(msg) from e

    def get_connected_devices(self) -> list[ConnectedDevice]:
        """Get list of currently connected Bluetooth devices."""
        stdout, _ = self._bluetoothctl(
            "bluetoothctl devices Connected",
            "Failed to list connected devices",
        )

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
        stdout, _ = self._bluetoothctl(
            f"bluetoothctl info {mac_address}",
            f"Failed to query device {mac_address}",
        )

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
        self._bluetoothctl(
            f"bluetoothctl connect {mac_address}",
            f"Failed to connect to {mac_address}",
        )
        time.sleep(0.5)  # Wait for connection to establish

    def disconnect_device(self, mac_address: str) -> None:
        """Disconnect a Bluetooth device."""
        self._bluetoothctl(
            f"bluetoothctl disconnect {mac_address}",
            f"Failed to disconnect {mac_address}",
        )

    def is_device_connected(self, mac_address: str) -> bool:
        """Check if a device is currently connected."""
        stdout, _ = self._bluetoothctl(
            f"bluetoothctl info {mac_address}",
            f"Failed to query device {mac_address}",
        )
        return "Connected: yes" in stdout
