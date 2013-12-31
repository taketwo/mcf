module MCF.Icons
  ( getIcon
  ) where

import MCF.Paths

getIcon :: String -> String
getIcon icon = "^i(" ++ pathIcons ++ icon ++ ".xbm)"
