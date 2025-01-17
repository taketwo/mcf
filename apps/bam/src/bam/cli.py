"""Command-line interface for Bluetooth Audio Manager."""

from __future__ import annotations

import logging
from pathlib import Path

import click

from .core import BTManager
from .types import AudioMode

logger = logging.getLogger(__name__)


def get_manager() -> BTManager:
    """Create BTManager with default config path."""
    config_path = Path.home() / ".config" / "bam" / "config.yaml"
    return BTManager(config_path)


@click.group()
@click.option("--debug", is_flag=True, help="Enable debug logging")
def cli(*, debug: bool) -> None:
    """BAM - Bluetooth Audio Manager."""
    if debug:
        logger.setLevel(logging.DEBUG)


@cli.command()
def status() -> None:
    """Show current system status."""
    manager = get_manager()
    status_info = manager.get_device_status()

    # Connected Devices Section
    click.echo("Connected devices:")
    if not status_info["connected"]:
        click.echo("  No devices connected")
    else:
        for device in status_info["connected"]:
            click.echo(f"\n  {device['name']}")
            click.echo(f"    MAC: {device['mac_address']}")
            click.echo(f"    Enrolled: {'yes' if device['is_known'] else 'no'}")
            mode_str = (
                device["current_mode"].name.lower()
                if device["current_mode"]
                else "unknown"
            )
            click.echo(f"    Mode: {mode_str}")
            if device["battery_level"] is not None:
                click.echo(f"    Battery: {device['battery_level']}%")
            if device["is_known"]:
                click.echo(f"    Aliases: {', '.join(device['aliases'])}")
            if not device["is_known"]:
                click.echo("    Note: Use 'bam enroll' to manage this device")

    # Disconnected Known Devices Section
    click.echo("\nDisconnected known devices:")
    disconnected = [d for d in status_info["known"] if not d["is_connected"]]
    if not disconnected:
        click.echo("  No disconnected known devices")
    else:
        for device in disconnected:
            click.echo(f"\n  {device['name']}")
            click.echo(f"    MAC: {device['mac_address']}")
            click.echo(f"    Aliases: {', '.join(device['aliases'])}")
            modes_str = ", ".join(m.name.lower() for m in device["supported_modes"])
            click.echo(f"    Supported modes: {modes_str}")
            click.echo(f"    Default mode: {device['default_mode'].name.lower()}")


@cli.command()
@click.argument("device_name", required=False)
@click.option(
    "--music",
    "mode",
    flag_value=AudioMode.MUSIC,
    help="Connect in high-quality audio mode",
)
@click.option(
    "--call",
    "mode",
    flag_value=AudioMode.CALL,
    help="Connect in bidirectional audio mode",
)
def connect(device_name: str | None, mode: AudioMode | None) -> None:
    """Connect to a device and set it as default audio device.

    If mode not specified, uses device's default mode from config.
    """
    manager = get_manager()
    device = manager.find_device(device_name)

    if not device:
        if device_name:
            click.echo(f"Device '{device_name}' not found in known devices")
        else:
            click.echo(
                "No default device found. Please specify device name or enroll a device.",
            )
        return

    manager.connect_device(device, mode)


@cli.command()
@click.argument("device_name", required=False)
def disconnect(device_name: str | None) -> None:
    """Disconnect a device."""
    manager = get_manager()
    device = manager.find_device(device_name)

    if not device:
        if device_name:
            click.echo(f"Device '{device_name}' not found in known devices")
        else:
            click.echo("No default device found. Please specify device name.")
        return

    manager.disconnect_device(device)


@cli.command()
@click.argument("device_name", required=False)
@click.option(
    "--music",
    "mode",
    flag_value=AudioMode.MUSIC,
    help="Switch to high-quality audio",
)
@click.option(
    "--call",
    "mode",
    flag_value=AudioMode.CALL,
    help="Switch to bidirectional audio",
)
def mode(device_name: str | None, mode: AudioMode | None) -> None:
    """Switch device mode.

    If no mode specified, toggles between music/call. If no device specified, acts on
    the only connected device.
    """
    manager = get_manager()
    device = manager.find_device(device_name)

    if not device:
        if device_name:
            click.echo(f"Device '{device_name}' not found in known devices")
        else:
            click.echo("No default device found. Please specify device name.")
        return

    if mode:
        manager.set_device_mode(device, mode)
    else:
        manager.toggle_mode(device)


@cli.command()
@click.option("--alias", help="Initial alias for the device")
def enroll(alias: str | None) -> None:
    """Add currently connected device to known devices."""
    manager = get_manager()

    connected = manager.bluetooth.get_connected_devices()
    known_macs = set(manager.devices.keys())
    unenrolled = [d for d in connected if d.mac_address not in known_macs]

    if not unenrolled:
        click.echo("No new devices to enroll")
        return

    device_info = None
    click.echo("Found new devices that can be enrolled:")
    for idx, device in enumerate(unenrolled, 1):
        click.echo(f"  {idx}. {device.mac_address} {device.name}")
    if len(unenrolled) > 1:
        choice = click.prompt(
            "Select device to enroll (enter number)",
            type=click.IntRange(1, len(unenrolled)),
            show_choices=False,
        )
        device_info = unenrolled[choice - 1]
    else:
        device_info = unenrolled[0]

    if not alias:
        default_alias = device_info.name.lower().replace(" ", "")
        alias = click.prompt(
            "Enter alias for quick access",
            default=default_alias,
            type=str,
        )

    # Detect supported modes by checking available profiles
    card_info = manager.pulseaudio._get_card_info(device_info.mac_address)
    modes = []
    if card_info:
        if "a2dp-sink" in card_info.lower():
            modes.append(AudioMode.MUSIC)
        if any(
            profile in card_info.lower()
            for profile in ["headset-head-unit", "handsfree-head-unit"]
        ):
            modes.append(AudioMode.CALL)

    # Set default mode
    mode_names = [m.name.lower() for m in modes]
    default_mode = AudioMode.MUSIC if AudioMode.MUSIC in modes else modes[0]
    chosen_mode = click.prompt(
        "Select default mode",
        type=click.Choice(mode_names),
        default=default_mode.name.lower(),
    )

    # Save to config
    manager.add_device(
        mac_address=device_info.mac_address,
        name=device_info.name,
        aliases=[alias] if alias else [],
        supported_modes=modes,
        default_mode=AudioMode[chosen_mode.upper()],
    )

    click.echo("Device enrolled successfully")


if __name__ == "__main__":
    cli()
