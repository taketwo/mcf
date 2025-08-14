"""Configuration management for nvim-manager."""

import os
import tomllib
from pathlib import Path

from typing import Annotated

from pydantic import BaseModel, BeforeValidator, ConfigDict

from .logging import get_logger

logger = get_logger(__name__)


def expand_path_validator(v: str | Path) -> Path:
    """Expand environment variables in string paths and resolve them."""
    if isinstance(v, str):
        return Path(os.path.expandvars(v)).expanduser().resolve()
    if isinstance(v, Path):
        return v
    msg = f"Expected string or Path, got {type(v)}"
    raise ValueError(msg)


ExpandedPath = Annotated[Path, BeforeValidator(expand_path_validator)]


class ConfigLoadError(Exception):
    """Raised when configuration cannot be loaded from a file."""

    @classmethod
    def from_path(cls, path: Path) -> "ConfigLoadError":
        """Create error for failed load from specific path."""
        return cls(f"Could not load config from {path}")

    @classmethod
    def no_config_found(cls) -> "ConfigLoadError":
        """Create error for when no config file can be found."""
        return cls("No config file found")


class LockRepositoryConfig(BaseModel):
    """Configuration for the lock repository.

    Attributes
    ----------
    https_uri : str
        HTTPS URI for cloning the lock repository.
    ssh_uri : str
        SSH URI for pushing to the lock repository.
    path : Path
        Local filesystem path where lock repository is stored.

    """

    model_config = ConfigDict(frozen=True)

    https_uri: str
    ssh_uri: str
    path: ExpandedPath


class EditorConfig(BaseModel):
    """Configuration for Neovim editor management.

    Attributes
    ----------
    install_path : Path
        Path where Neovim binary will be installed.
    build_cache : Path
        Directory for storing Neovim source code and build artifacts.
    lock_file : str
        Name of the lock file in the repository.
    repository : str
        GitHub repository in format "owner/name" for Neovim source.

    """

    model_config = ConfigDict(frozen=True)

    install_path: ExpandedPath
    build_cache: ExpandedPath
    lock_file: str = "neovim-lock.json"
    repository: str = "neovim/neovim"


class PluginsConfig(BaseModel):
    """Configuration for plugin management.

    Attributes
    ----------
    restore_retry_count : int
        Number of times to retry plugin restoration on failure.
    local_lock_path : Path
        Path to local Lazy plugin manager lock file (lazy-lock.json).

    """

    model_config = ConfigDict(frozen=True)

    restore_retry_count: int = 3
    local_lock_path: ExpandedPath


class Config(BaseModel):
    """Main configuration for nvim-manager.

    Attributes
    ----------
    lock_repository : LockRepositoryConfig
        Configuration for lock repository operations.
    editor : EditorConfig
        Configuration for Neovim editor management.
    plugins : PluginsConfig
        Configuration for plugin management.

    """

    model_config = ConfigDict(validate_assignment=True, frozen=False)

    lock_repository: LockRepositoryConfig
    editor: EditorConfig
    plugins: PluginsConfig

    @classmethod
    def from_path_or_default(cls, path: Path | None = None) -> "Config":
        """Create configuration from a file, falling back to default location.

        Parameters
        ----------
        path : Path | None
            Path to configuration file. If None, tries default location.

        Returns
        -------
        Config
            Loaded configuration

        Raises
        ------
        ConfigLoadError
            If configuration cannot be loaded from either path.

        """
        if path:
            return cls.from_path(path)

        default_path = Path.home() / ".config" / "nvim-manager" / "config.toml"
        if default_path.is_file():
            return cls.from_path(default_path)

        raise ConfigLoadError.no_config_found()

    @classmethod
    def from_path(cls, path: Path) -> "Config":
        """Create configuration from a file.

        Parameters
        ----------
        path : Path
            Path to configuration file.

        Returns
        -------
        Config
            Loaded configuration.

        Raises
        ------
        ConfigLoadError
            If file cannot be read or parsed.

        """
        try:
            with path.open("rb") as f:
                config_data = tomllib.load(f)
            config = cls.model_validate(config_data)
        except Exception as e:
            raise ConfigLoadError.from_path(path) from e
        else:
            logger.debug("Loaded configuration from %s", path)
            return config


def load_config(config_path: Path | None = None) -> Config:
    """Load configuration from TOML file.

    Parameters
    ----------
    config_path : Path, optional
        Path to config file. If None, defaults to ~/.config/nvim-manager/config.toml.

    Returns
    -------
    Config
        Loaded and validated configuration object.

    Raises
    ------
    FileNotFoundError
        If config file doesn't exist at the specified path.
    tomllib.TOMLDecodeError
        If config file contains invalid TOML syntax.
    pydantic.ValidationError
        If config values fail validation (e.g., invalid paths, missing required fields).

    """
    if config_path is None:
        config_path = Path.home() / ".config" / "nvim-manager" / "config.toml"

    if not config_path.exists():
        msg = f"Configuration file not found: {config_path}"
        raise FileNotFoundError(msg)

    with config_path.open("rb") as f:
        config_data = tomllib.load(f)

    return Config(**config_data)
