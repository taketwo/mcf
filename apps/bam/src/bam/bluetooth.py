"""Bluetooth system interface using bluetoothctl."""

from __future__ import annotations

import time
from dataclasses import dataclass

from .logging import get_logger
from .utils import run_command

logger = get_logger(__name__)


@dataclass
class ConnectedDevice:
    """Information about a connected Bluetooth device."""

    mac_address: str
    name: str


class BluetoothController:
    """Interface to system Bluetooth functionality via bluetoothctl."""

    def get_connected_devices(self) -> list[ConnectedDevice]:
        """Get list of currently connected Bluetooth devices."""
        stdout, _, _ = run_command("bluetoothctl devices Connected")
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
        stdout, _, _ = run_command(f"bluetoothctl info {mac_address}")

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
        run_command(f"bluetoothctl connect {mac_address}")
        time.sleep(0.5)  # Wait for connection to establish

    def disconnect_device(self, mac_address: str) -> None:
        """Disconnect a Bluetooth device."""
        run_command(f"bluetoothctl disconnect {mac_address}")

    def is_device_connected(self, mac_address: str) -> bool:
        """Check if a device is currently connected."""
        stdout, _, _ = run_command(f"bluetoothctl info {mac_address}")
        return "Connected: yes" in stdout
