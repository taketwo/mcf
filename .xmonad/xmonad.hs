--- Large sections of this file come from
--- github.com/davidbrewer/xmonad-ubuntu-conf

import XMonad

import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))

import Solarized

{-
  Xmonad configuration variables.
-}

mcfModMask            = mod4Mask        -- changes the mod key to "super"
mcfFocusedBorderColor = solarizedOrange -- color of focused border
mcfNormalBorderColor  = solarizedBase03 -- color of inactive border
mcfBorderWidth        = 1               -- width of border around windows

{-
  Layout configuration.
-}

defaultLayouts = smartBorders $ avoidStruts $
      windowNavigation mouseResizableTile
        { draggerType = BordersDragger }
  ||| windowNavigation mouseResizableTile
        { draggerType = BordersDragger
        , isMirrored = True }
  ||| windowNavigation Grid
  ||| noBorders Full

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
mcfLayouts =
  {-onWorkspace "7:Chat" chatLayout-}
  {-$ onWorkspace "9:Pix" gimpLayout-}
  {-$-} defaultLayouts

myManageHook :: [ManageHook]
myManageHook =
  [ resource  =? "Do" --> doIgnore
  , isFullscreen --> doFullFloat
  , className =? "Unity-2d-panel" --> doIgnore ]

main = xmonad $ gnomeConfig
  { modMask = mcfModMask
  , focusedBorderColor = mcfFocusedBorderColor
  , normalBorderColor = mcfNormalBorderColor
  , terminal = "urxvt"
  , borderWidth = mcfBorderWidth
  , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
  , logHook = ewmhDesktopsLogHook >> setWMName "LG3D" -- java workaround
  , layoutHook = mcfLayouts-- $ layoutHook gnomeConfig
  }
  `additionalKeysP`
    [ ("M-S-q", spawn "gnome-session-save --gui --logout-dialog") -- display logout-dialog
    -- Lock Screen
    , ("M-S-l", spawn "gnome-screensaver-command -l")
    {-, ("<XF86Forward>", nextWS)-}
    {-, ("<XF86Back>", prevWS)-}
    , ("M1-<Tab>", windows W.focusUp)
    , ("M-<L>", prevScreen)
    , ("M-<R>", nextScreen)
    , ("M-c", sendMessage $ Go U)
    , ("M-t", sendMessage $ Go D)
    , ("M-h", sendMessage $ Go L)
    , ("M-n", sendMessage $ Go R)
    , ("M-C-<L>", sendMessage Shrink)
    , ("M-C-<R>", sendMessage Expand)
    , ("M-C-c", sendMessage $ Swap U)
    , ("M-C-t", sendMessage $ Swap D)
    , ("M-C-h", sendMessage $ Swap L)
    , ("M-C-n", sendMessage $ Swap R)
    , ("M-C-m", windows W.swapMaster)
    , ("M-C-f", withFocused $ windows . W.sink)
    ]
