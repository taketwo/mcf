module MCF.Apps
  ( appTerminal
  , appBrowser
  , appTmux
  , spawnInTerminal
  ) where

import XMonad.Core

-- Currently Xmonad takes care of starting tmux
-- Consequently, if shell is spawned not through Xmonad (ssh, non-DM login), tmux is not started
-- Resolution: exec tmux in .bashrc in *interactive* mode (see https://unix.stackexchange.com/a/113768/55482)

appTerminal = "WINIT_X11_SCALE_FACTOR=1 alacritty"
appTmux = appTerminal ++ " -e 'tmux'"
appBrowser  = "browser"

spawnInTerminal :: String -> X ()
spawnInTerminal cmd = spawn $ appTerminal ++ " -e '" ++ cmd ++ "'"
