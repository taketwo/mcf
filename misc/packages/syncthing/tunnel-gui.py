#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "pyyaml",
# ]
# ///

import socket
import subprocess
import sys
import time
import webbrowser
from pathlib import Path

import yaml

CONFIG_FILE = "config.yaml"
REMOTE_PORT = 8384
LOCAL_PORT = 8385


def load_config() -> dict:
    config_path = Path(__file__).parent / CONFIG_FILE
    with config_path.open("r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def find_free_port(start_port: int) -> int:
    """Find a free local port starting from start_port."""
    port = start_port
    while port < start_port + 100:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            try:
                sock.bind(("127.0.0.1", port))
                return port
            except OSError:
                port += 1
    raise RuntimeError(f"Could not find free port starting from {start_port}")


def main() -> None:
    if len(sys.argv) != 2:
        print("Usage: tunnel-gui.py <hostname>")
        sys.exit(1)

    target_host = sys.argv[1]
    current_host = socket.gethostname().split(".")[0].lower()

    if target_host == current_host:
        print(f"Error: target host '{target_host}' is the current host")
        sys.exit(1)

    config = load_config()
    if target_host not in config["hosts"]:
        print(f"Error: host '{target_host}' not found in config")
        sys.exit(1)

    host_config = config["hosts"][target_host]
    if not host_config.get("addresses"):
        print(f"Error: no addresses configured for host '{target_host}'")
        sys.exit(1)

    # Use the first configured address
    remote_address = host_config["addresses"][0]

    # Find a free local port
    local_port = find_free_port(LOCAL_PORT)

    print(f"Creating SSH tunnel to {target_host} ({remote_address})...")
    print(f"Local port: {local_port}")
    print(f"Remote port: {REMOTE_PORT}")
    print("Press Ctrl+C to close the tunnel and exit")

    # Create SSH tunnel
    ssh_command = [
        "ssh",
        "-N",  # Don't execute remote command
        "-L",
        f"{local_port}:localhost:{REMOTE_PORT}",
        remote_address,
    ]

    try:
        # Start SSH tunnel in background
        tunnel_process = subprocess.Popen(
            ssh_command,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )

        # Give SSH a moment to establish the tunnel
        time.sleep(1)

        # Check if SSH process is still running
        if tunnel_process.poll() is not None:
            stderr = (
                tunnel_process.stderr.read().decode() if tunnel_process.stderr else ""
            )
            print(f"Error: SSH tunnel failed to establish")
            if stderr:
                print(f"SSH error: {stderr}")
            sys.exit(1)

        # Open browser
        url = f"http://localhost:{local_port}"
        print(f"Opening browser to {url}...")
        webbrowser.open(url)

        # Wait for user to terminate
        tunnel_process.wait()

    except KeyboardInterrupt:
        print("\nClosing tunnel...")
        tunnel_process.terminate()
        tunnel_process.wait()
    except Exception as e:
        print(f"Error: {e}")
        if tunnel_process.poll() is None:
            tunnel_process.terminate()
        sys.exit(1)


if __name__ == "__main__":
    main()

# vim: set ft=python:
