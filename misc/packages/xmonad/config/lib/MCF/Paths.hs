module MCF.Paths
  ( pathHome
  , pathMCF
  , pathXmonad
  , pathPolybar
  , pathIcons
  ) where

import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath (joinPath)
import System.IO.Unsafe

pathHome = unsafePerformIO $ getHomeDirectory
pathMCF = unsafePerformIO $ getEnv "MCF"
pathXmonad = joinPath [pathHome, ".xmonad/"]
pathPolybar = joinPath [pathXmonad, "spawn_polybar.sh"]
pathIcons = joinPath [pathXmonad, "icons/"]
