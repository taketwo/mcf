module MCF.Paths
  ( pathHome
  , pathXmonad
  , pathIcons
  ) where

import System.IO.Unsafe
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

pathHome = unsafePerformIO $ getHomeDirectory
pathXmonad = joinPath [pathHome, ".xmonad/"]
pathIcons = joinPath [pathXmonad, "icons/"]
