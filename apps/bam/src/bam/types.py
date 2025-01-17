"""Common types and enums for Bluetooth Audio Manager."""

from enum import auto, Enum


class AudioMode(Enum):
    """Available audio modes for Bluetooth devices."""

    MUSIC = auto()  # A2DP high-quality mode
    CALL = auto()  # HSP/HFP bidirectional mode
