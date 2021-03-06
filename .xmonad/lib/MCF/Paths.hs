module MCF.Paths
  ( pathHome
  , pathXmonad
  , pathPolybar
  , pathIcons
  ) where

import System.IO.Unsafe
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

pathHome = unsafePerformIO $ getHomeDirectory
pathXmonad = joinPath [pathHome, ".xmonad/"]
pathPolybar = joinPath [pathXmonad, "spawn_polybar.sh"]
pathIcons = joinPath [pathXmonad, "icons/"]
