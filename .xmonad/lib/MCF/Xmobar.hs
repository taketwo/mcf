module MCF.Xmobar
  ( xmobarAction
  , xmobarIcon
  ) where

import MCF.Paths

xmobarAction :: String -> Int -> String -> String
xmobarAction cmd btn txt = "<action=`" ++ cmd ++ "` button=" ++ show btn ++ ">" ++ txt ++ "</action>"

xmobarIcon :: String -> String
xmobarIcon icon = "<icon=" ++ pathIcons ++ icon ++ ".xpm/>"

