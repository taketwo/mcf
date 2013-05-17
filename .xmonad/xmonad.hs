--- Large sections of this file come from
--- github.com/davidbrewer/xmonad-ubuntu-conf

-- Language
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses,  ImplicitParams, PatternGuards #-}

import XMonad
import XMonad.Core
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders (noBorders,smartBorders,withBorder)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.WindowNavigation
import XMonad.StackSet (RationalRect (..), currentTag)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Man
import XMonad.Util.Timer
import XMonad.Util.Cursor
import XMonad.Util.Loggers
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad
import XMonad.Core
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Actions.CycleWS
import qualified XMonad.Hooks.EwmhDesktops as ED
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
import XMonad.Util.Timer
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import Data.List
import Data.IORef
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86

import System.Exit
import System.IO
import System.Directory
import System.Environment

import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import System.Exit
import System.IO (Handle, hPutStrLn)
{-import Control.Exception as E-}
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Util.ExtensibleState as XS
import System.IO (Handle, hPutStrLn)

import Solarized

{-
  Xmonad configuration variables.
-}

-- Configuration ----------------------------------------------------------- {{{

-- }}}
-- Workspaces -------------------------------------------------------------- {{{

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

-- }}}

-- Appearance -------------------------------------------------------------- {{{

dzenFont             = "-*-CPMono_v07 Plain for Powerline-*-r-normal-*-11-*-*-*-*-*-*-*"
dzenBg               = solarizedBase03
colorBlack           = "#020202" --Background (Dzen_BG)
colorBlackAlt        = "#1c1c1c" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#101010" --Gray dark
colorWhite           = "#a9a6af" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#44aacc"
colorBlueAlt         = "#3955c4"
colorRed             = "#e0105f"
colorGreen           = "#66ff66"
colorGreenAlt        = "#558965"
myNormalBorderColor  = colorBlackAlt
myFocusedBorderColor = colorGray
myIconPath           = "/home/sergey/.xmonad/icons/"
myBinPath            = "~/.xmonad/icons/"
xRes                 = 166
yRes                 = 768
panelHeight          = 16
panelBoxHeight       = 12

-- }}}
-- Layouts ----------------------------------------------------------------- {{{

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

-- }}}

myManageHook :: [ManageHook]
myManageHook =
  [ isFullscreen --> doFullFloat
  , isDialog --> doFloat
  , className =? "Skype" --> doF (W.shift "8")
  , className =? "Workrave" --> doF (W.shift "9")
  , className =? "Rhythmbox" --> doF (W.shift "9")
  ]

main :: IO ()
main = do
  home    <- catch (getEnv "HOME") ( const $ return [])
  d       <- catch (getEnv "DISPLAY") ( const $ return [])
  display <- openDisplay d
  let screen       = defaultScreenOfDisplay display
  let screenWidth  = read (show (widthOfScreen screen))  :: Int
  let screenHeight = read (show (heightOfScreen screen)) :: Int
  let barTopLeft   = "dzen2"
                     ++ " -x '0' -y '0'"
                     ++ " -h '16' -w '400'"
                     ++ " -ta 'l'"
                     ++ " -fg '" ++ colorWhiteAlt ++ "'"
                     ++ " -bg '" ++ dzenBg ++ "'"
                     ++ " -fn '" ++ dzenFont ++ "'"
  let barTopRight  = "dzen2"
                     ++ " -x '400' -y '0'"
                     ++ " -h '16' -w '" ++ show (screenWidth - 400) ++ "'"
                     ++ " -ta 'r'"
                     ++ " -fg '" ++ colorWhiteAlt ++ "'"
                     ++ " -bg '" ++ dzenBg ++ "'"
                     ++ " -fn '" ++ dzenFont ++ "'"

  dzenTopLeft  <- spawnPipe barTopLeft
  dzenTopRight <- spawnPipe barTopRight
  {-topRightBar <- spawnPipe mcfXmonadBar-}
  {-dzenLeftBar <- spawnPipe mcfXmonadBar-}
  xmonad $ gnomeConfig
    { modMask            = mod1Mask         -- changes the mode key to "super"
    , focusedBorderColor = solarizedOrange  -- color of focused border
    , normalBorderColor  = solarizedBase03  -- color of inactive border
    , borderWidth        = 1                -- width of border around windows
    , terminal           = "gnome-terminal" -- default terminal program
    , workspaces         = myWorkspaces
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    {-, logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd-}
    , logHook = logHookTopLeft dzenTopLeft <+> logHookTopRight dzenTopRight
    , layoutHook = mcfLayouts-- $ layoutHook gnomeConfig
    , handleEventHook    = myHandleEventHook
    , startupHook        = startTimer 1 >>= XS.put . TID
    }
    `additionalKeysP`
    [ ("M-S-q", spawn "gnome-session-save --gui --logout-dialog") -- display logout-dialog
    , ("M1-<F10>", spawn "gnome-screensaver-command -l")
    , ("M1-<F11>", spawn "pm-hybernate")
    , ("M1-<F12>", spawn "pm-suspend")
    {-, ("<XF86Forward>", nextWS)-}
    {-, ("<XF86Back>", prevWS)-}
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-a", sendMessage NextLayout)
    , ("M1-<Tab>", windows W.focusDown)
    , ("M-<L>", prevScreen)
    , ("M-<R>", nextScreen)
    , ("M-c", sendMessage $ Go U)
    , ("M-t", sendMessage $ Go D)
    , ("M-h", sendMessage $ Go L)
    , ("M-n", sendMessage $ Go R)
    , ("M-S-<L>", sendMessage Shrink)
    , ("M-S-<R>", sendMessage Expand)
    , ("M-S-c", sendMessage $ Swap U)
    , ("M-S-t", sendMessage $ Swap D)
    , ("M-S-h", sendMessage $ Swap L)
    , ("M-S-n", sendMessage $ Swap R)
    , ("M-S-<Space>", windows W.swapMaster)
    , ("M-S-w", kill)
    , ("M1-<F4>", kill)
    , ("M-S-f", withFocused $ windows . W.sink)
    -- Application shortcuts
    , ("M-<Return>", spawn "gnome-terminal")
    , ("M-<Space>", spawn "kupfer")
    , ("M-r", spawn "rhythmbox")
    , ("M-q", restart "xmonad" True)                           --Restart xmonad
    , ("M-b", spawn "chromium-browser")
    ]


-- Status bars ------------------------------------------------------------- {{{
-- Top left (XMonad status) ------------------------------------------------ {{{
logHookTopLeft :: Handle -> X ()
logHookTopLeft h = dynamicLogWithPP $ defaultPP
  { ppOutput          = hPutStrLn h
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP) --hide "NSP" from workspace list
  {-, ppOrder           = \(ws:l:t:x) -> [ws, l, t] ++ x-}
  , ppSep             = " "
  , ppWsSep           = ""
  , ppCurrent         = wrapTextBox solarizedBase02 solarizedBase1  colorBlack
  , ppUrgent          = wrapTextBox solarizedRed    solarizedBase02 colorBlack -- . wrapClickWorkspace
  , ppVisible         = wrapTextBox solarizedBlue   solarizedBase02 colorBlack -- . wrapClickWorkspace
  , ppHiddenNoWindows = wrapTextBox solarizedBase03 solarizedBase02 colorBlack -- . wrapClickWorkspace
  , ppHidden          = wrapTextBox solarizedBase1  solarizedBase02 colorBlack -- . wrapClickWorkspace
  , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
  }

{-logHookTopLeft h = dynamicLogWithPP $ defaultPP-}
  {-{ ppOutput            =   hPutStrLn h-}
  {-, ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad-}
  {-, ppVisible           =   dzenColor "white" "#1B1D1E" . pad-}
  {-, ppHidden            =   dzenColor "white" "#1B1D1E" . pad-}
  {-, ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad-}
  {-, ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad-}
  {-, ppWsSep             =   " "-}
  {-, ppSep               =   "  |  "-}
  {-, ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .-}
                            {-(\x -> case x of-}
                                {-"MouseResizableTile"        ->      "^i(" ++ myIconPath ++ "tall.xbm)"-}
                                {-"Mirror ResizableTall"      ->      "^i(" ++ myIconPath ++ "mtall.xbm)"-}
                                {-"Full"                      ->      "^i(" ++ myIconPath ++ "full.xbm)"-}
                                {-"Simple Float"              ->      "~"-}
                                {-_                           ->      x-}
                            {-)-}
  {-, ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape-}
  {-}-}

-- }}}
-- Top right (date and time) ----------------------------------------------- {{{

logHookTopRight :: Handle -> X ()
logHookTopRight h = dynamicLogWithPP $ defaultPP
  { ppOutput          = hPutStrLn h
  , ppOrder           = \(_:_:_:x) -> x
  , ppSep             = " "
  , ppExtras          = [ date $ (wrapTextBox colorBlack colorWhiteAlt colorBlack "%A") ++ (wrapTextBox colorWhiteAlt colorGrayAlt colorBlack $ "%Y^fg(" ++ colorGray ++ ").^fg()%m^fg(" ++ colorGray ++ ").^fg()^fg(" ++ colorBlue ++ ")%d^fg() ^fg(" ++ colorGray ++ ")-^fg() %H^fg(" ++ colorGray ++"):^fg()%M^fg(" ++ colorGray ++ "):^fg()^fg(" ++ colorGreen ++ ")%S^fg()") ]
  }

-- Wrap Box
wrapTextBox :: String -> String -> String -> String -> String
wrapTextBox fg bg1 bg2 t = "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath  ++ "boxleft.xbm)^ib(1)^r(" ++ show xRes ++ "x" ++ show panelBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg(" ++ fg ++ ")" ++ t ++ "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath ++ "boxright.xbm)^fg(" ++ bg2 ++ ")^r(" ++ show xRes ++ "x" ++ show panelBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg()^ib(0)"

-- }}}
-- }}}

-- HandleEvent hook -------------------------------------------------------- {{{

-- Wrapper for the Timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
  initialValue = TID 0

myHandleEventHook = ED.fullscreenEventHook <+> docksEventHook <+> clockEventHook <+> handleTimerEvent
  where
    clockEventHook e = do                   -- thanks to DarthFennec
      (TID t) <- XS.get                     -- get the recent Timer id
      handleTimer t e $ do                  -- run the following if e matches the id
        startTimer 1 >>= XS.put . TID       -- restart the timer, store the new id
        ask >>= logHook.config              -- get the loghook and run it
        return Nothing                      -- return required type
      return $ All True                     -- return required type

-- }}}
