from pathlib import Path
from typing import Union

HOME: Path = Path.home()
MCF: Path = HOME / ".mcf"
XMONAD: Path = HOME / ".xmonad"


def path(*args: Union[str, Path]) -> Path:
    """Create a path relative to MCF directory.

    Parameters
    ----------
    args: List[Union[str, Path]]
        A list of strings or Path objects representing the path

    Returns
    -------
    Path: A Path object representing the full path relative to MCF directory.

    Example:
        >>> path("scripts", "library")
        PosixPath('/home/user/.mcf/scripts/library')

    """
    return MCF.joinpath(*args)
