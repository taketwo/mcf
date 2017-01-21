module MCF.Apps
  ( appTerminal
  , appBrowser
  , appShell
  , appBrowse
  , appEdit
  ) where

import XMonad.Core

appTerminal = "gnome-terminal"
appBrowser  = "chromium-browser"
appShell    = "bash"

appEdit :: String -> X ()
appEdit f = spawn (appTerminal ++ " -e 'vim " ++ f ++ "'")

appBrowse :: String -> X ()
appBrowse f = spawn (appBrowser ++ " " ++ f)
