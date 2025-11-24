#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "pyyaml",
#     "requests",
# ]
# ///

import json
import os
import socket
import sys
from pathlib import Path
from urllib.parse import urljoin

import requests
import yaml

CONFIG_FILE = "config.yaml"
API_KEY_HEADER = "X-API-Key"
API_URL = "http://127.0.0.1:8384"
DEFAULT_PROTOCOL = "tcp"
DEFAULT_PORT = "22000"


def load_config() -> dict:
    config_path = Path(__file__).parent / CONFIG_FILE
    with config_path.open("r", encoding="utf-8") as f:
        return yaml.safe_load(f)


class SyncthingClient:
    """Minimal client wrapper for the Syncthing REST API."""

    def __init__(self, url: str, api_key: str, timeout=10):
        self._base_url = url if url.endswith("/") else url + "/"
        self._api_key = api_key
        self._timeout = timeout
        self._headers = {
            "X-API-Key": self._api_key,
            "Content-Type": "application/json",
        }

    def _request(self, method: str, path: str, payload=None, **kwargs):
        resp = requests.request(
            method=method.upper(),
            url=urljoin(self._base_url, path),
            headers=self._headers,
            timeout=self._timeout,
            json=payload,
            **kwargs,
        )
        resp.raise_for_status()
        if not resp.text:
            return None
        try:
            return resp.json()
        except ValueError:
            return resp.text

    def get(self, path: str, **kwargs):
        return self._request("GET", path, **kwargs)

    def put(self, path: str, payload=None, **kwargs):
        return self._request("PUT", path, payload=payload, **kwargs)

    def post(self, path: str, payload=None, **kwargs):
        return self._request("POST", path, payload=payload, **kwargs)

    def patch(self, path: str, payload=None, **kwargs):
        return self._request("PATCH", path, payload=payload, **kwargs)

    def get_config(self):
        return self.get("rest/config")

    def replace_config(self, config_dict):
        return self.put("rest/config", payload=config_dict)

    def list_folders(self):
        return self.get("rest/config/folders")

    def replace_folders(self, folders_array):
        return self.put("rest/config/folders", payload=folders_array)

    def add_folder(self, folder_obj):
        return self.post("rest/config/folders", payload=folder_obj)

    def update_folder(self, folder_id, folder_obj):
        return self.put(f"rest/config/folders/{folder_id}", payload=folder_obj)

    def patch_folder(self, folder_id, patch_obj):
        return self.patch(f"rest/config/folders/{folder_id}", payload=patch_obj)

    def restart_required(self):
        return self.get("rest/config/restart-required")


def main() -> None:
    config = load_config()
    current_host = socket.gethostname().split(".")[0].lower()
    if current_host not in config["hosts"]:
        print(f"Host '{current_host}' not found in config, aborting")
        sys.exit(1)
    host_config = config["hosts"][current_host]

    client = SyncthingClient(
        url=API_URL,
        api_key=config["global"]["api_key"],
    )

    # Global configuration

    client.patch(
        "rest/config/options",
        {
            "listenAddresses": ["tcp://0.0.0.0:22000"],
            "globalAnnounceEnabled": False,
            "localAnnounceEnabled": False,
            "protocolIncomingConnection": "tcp",
            "protocolOutgoingConnection": "tcp",
        },
    )

    # Devices configuration

    existing_devices = {d["deviceID"]: d for d in client.get("rest/config/devices")}
    new_devices = []

    for peer_name in host_config["peers"]:
        peer_device_id = config["hosts"][peer_name]["device_id"]
        peer_addresses = [
            f"{DEFAULT_PROTOCOL}://{addr}:{DEFAULT_PORT}"
            for addr in config["hosts"][peer_name]["addresses"]
        ]
        updated = {
            **existing_devices.get(peer_device_id, {}),
            "deviceID": peer_device_id,
            "name": peer_name,
            "addresses": peer_addresses,
            "compression": "metadata",
            "introducer": False,
        }
        new_devices.append(updated)

    client.put("rest/config/devices", payload=new_devices)

    # Folders configuration

    existing_folders = {f["id"]: f for f in client.get("rest/config/folders")}
    new_folders = []

    for folder_id, folder_data in host_config["folders"].items():
        member_device_ids = [
            {"deviceID": config["hosts"][host_name]["device_id"]}
            for host_name, host_data in config["hosts"].items()
            if folder_id in host_data.get("folders", {}) and host_name != current_host
        ]
        updated = {
            **existing_folders.get(folder_id, {}),
            "id": folder_id,
            "label": config["folders"][folder_id],
            "path": os.path.expandvars(folder_data["path"]),
            "type": folder_data["type"],
            "devices": member_device_ids,
            "rescanIntervalS": 300,
        }
        new_folders.append(updated)

    client.put("rest/config/folders", payload=new_folders)

    if client.restart_required():
        print("Restarting Syncthing to apply new configuration...")
        client.post("rest/system/restart")


if __name__ == "__main__":
    main()

# vim: set ft=python:
