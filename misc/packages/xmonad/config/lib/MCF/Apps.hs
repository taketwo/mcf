module MCF.Apps
  ( appTerminal
  , appBrowser
  , appShell
  , appBrowse
  , appEdit
  , appTmux
  ) where

import XMonad.Core

-- Currently Xmonad takes care of starting tmux
-- Consequently, if shell is spawned not through Xmonad (ssh, non-DM login), tmux is not started
-- Resolution: exec tmux in .bashrc in *interactive* mode (see https://unix.stackexchange.com/a/113768/55482)

appTerminal = "gnome-terminal -e 'tmux'"
appBrowser  = "browser"
appShell    = "bash"

appEdit :: String -> X ()
appEdit f = spawn (appTerminal ++ " -e 'vim " ++ f ++ "'")

appBrowse :: String -> X ()
appBrowse f = spawn (appBrowser ++ " " ++ f)

appTmux :: (String, String) -> X ()
appTmux (topic, dir) = spawn $ "gnome-terminal -e 'tmux new -s " ++ topic ++ "' --working-directory " ++ dir
