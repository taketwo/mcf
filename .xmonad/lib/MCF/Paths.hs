module MCF.Paths
  ( pathHome
  , pathXmonad
  , pathXmobar
  , pathIcons
  ) where

import System.IO.Unsafe
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

pathHome = unsafePerformIO $ getHomeDirectory
pathXmonad = joinPath [pathHome, ".xmonad/"]
pathXmobar = joinPath [pathXmonad, "xmobarrc"]
pathIcons = joinPath [pathXmonad, "icons/"]
