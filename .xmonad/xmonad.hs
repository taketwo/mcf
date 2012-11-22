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
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
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
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

mcfModMask            = mod4Mask        -- changes the mod key to "super"
mcfFocusedBorderColor = solarizedBlue   -- color of focused border
mcfNormalBorderColor  = solarizedBase01 -- color of inactive border
mcfBorderWidth        = 1               -- width of border around windows

{-
  Layout configuration. In this section we identify which xmonad
  layouts we want to use. I have defined a list of default
  layouts which are applied on every workspace, as well as
  special layouts which get applied to specific workspaces.

  Note that all layouts are wrapped within "avoidStruts". What this does
  is make the layouts avoid the status bar area at the top of the screen.
  Without this, they would overlap the bar. You can toggle this behavior
  by hitting "super-b" (bound to ToggleStruts in the keyboard bindings
  in the next section).
-}

-- Define group of default layouts used on most screens, in the
-- order they will appear.
-- "smartBorders" modifier makes it so the borders on windows only
-- appear if there is more than one visible window.
-- "avoidStruts" modifier makes it so that the layout provides
-- space for the status bar at the top of the screen.
defaultLayouts = smartBorders(avoidStruts(
  -- ResizableTall layout has a large master window on the left,
  -- and remaining windows tile on the right. By default each area
  -- takes up half the screen, but you can resize using "super-h" and
  -- "super-l".
  ResizableTall 1 (3/100) (1/2) []

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders Full

  -- Grid layout tries to equally distribute windows in the available
  -- space, increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| Grid

  -- ThreeColMid layout puts the large master window in the center
  -- of the screen. As configured below, by default it takes of 3/4 of
  -- the available space. Remaining windows tile to both the left and
  -- right of the master window. You can resize using "super-h" and
  -- "super-l".
  ||| ThreeColMid 1 (3/100) (3/4)

  -- Circle layout places the master window in the center of the screen.
  -- Remaining windows appear in a circle around it
  -- ||| Circle)--)
  ))


-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
mcfLayouts =
  {-onWorkspace "7:Chat" chatLayout-}
  {-$ onWorkspace "9:Pix" gimpLayout-}
  {-$-} defaultLayouts

myManageHook :: [ManageHook]
myManageHook =
  [ resource  =? "Do" --> doIgnore
    ,isFullscreen --> doFullFloat
    , className =? "Unity-2d-panel" --> doIgnore ]
--    , className =? "Unity-2d-shell" --> doFloat ]

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
    {-, ("M-p", spawn "kupfer")-}
    {-, ("M-<Space>", spawn "kupfer")-}
    , ("<XF86Forward>", nextWS)
    , ("<XF86Back>", prevWS)
    , ("M1-<Tab>", windows W.focusDown)
    , ("M-<R>", nextScreen)
    , ("M-<L>", prevScreen)
    ]
