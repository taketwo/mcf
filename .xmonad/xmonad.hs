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
import XMonad.Layout.IndependentScreens
import XMonad.StackSet (RationalRect (..), currentTag)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
{-import XMonad.Hooks.DynamicBars-}
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
import XMonad.Actions.UpdatePointer
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
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Util.ExtensibleState as XS
import System.IO (Handle, hPutStrLn)

import Solarized

-- Configuration ----------------------------------------------------------- {{{
myTerminal = "gnome-terminal"
-- }}}
-- Workspaces -------------------------------------------------------------- {{{

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

-- }}}
-- Appearance -------------------------------------------------------------- {{{

dzenFont             = "-*-CPMono_v07 Plain for Powerline-*-r-normal-*-11-*-*-*-*-*-*-*"
dzenBg               = "#3c3b37"
dzenFgLight          = "#d7dbd2"
dzenFgDark           = "#7f7d76"
dzenUrgent           = "#19b6ee"
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
xRes                 = 166
yRes                 = 768
panelHeight          = 16
panelBoxHeight       = 12
topLeftBarWidth      = 500

-- }}}
-- Layouts ----------------------------------------------------------------- {{{

defaultLayouts = smartBorders $ avoidStruts $
      windowNavigation mouseResizableTile
        { draggerType = BordersDragger }
  ||| windowNavigation mouseResizableTile
        { draggerType = BordersDragger
        , isMirrored  = True }
  ||| windowNavigation Grid
  ||| Full

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
mcfLayouts =
  {-onWorkspace "7:Chat" chatLayout-}
  {-$ onWorkspace "9:Pix" gimpLayout-}
  {-$-} defaultLayouts

-- }}}
-- Scratchpads ------------------------------------------------------------- {{{

myScratchPads = [ NS "terminal" spawnTerminal  findTerminal  manageTerminal ]
  where
    spawnTerminal  = myTerminal ++ " --disable-factory --name scratchpad"
    findTerminal   = resource  =? "scratchpad"
    manageTerminal = customFloating $ W.RationalRect l t w h
      where
        h = 0.3             -- height
        w = 1.0             -- width
        t = 1.0 - h         -- bottom edge
        l = (1.0 - w) / 2.0 -- centered left/right

-- }}}
-- Manage hook ------------------------------------------------------------- {{{

myManageHook :: ManageHook
myManageHook = manageWindows <+> manageDocks <+> namedScratchpadManageHook myScratchPads

manageWindows = composeAll . concat $
    [ [isDialog --> doFloat]
    , [className =? c --> doFloat  | c <- myCFloats]
    , [title     =? t --> doFloat  | t <- myTFloats]
    , [resource  =? r --> doFloat  | r <- myRFloats]
    , [resource  =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "1" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "2" | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "3" | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "4" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "5" | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "6" | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "7" | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "8" | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "9" | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = []
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = []
    my1Shifts = ["Chromium-browser"]
    my2Shifts = []
    my3Shifts = []
    my4Shifts = []
    my5Shifts = []
    my6Shifts = []
    my7Shifts = []
    my8Shifts = ["Skype"
                , "crx_kbpgddbgniojgndnhlkjbkpknjhppkbk" -- Google+ Hangouts application
                , "crx_nckgahadagoaajjgafhacjanaoiihapd" -- Google+ Hangouts extension
                ]
    my9Shifts = ["Rhythmbox", "Workrave"]

-- }}}
-- Main -------------------------------------------------------------------- {{{

{-myStatusBar :: DynamicStatusBar-}
{-myStatusBar = spawnPipe . ("~/.xmonad/print.sh " ++ ) . show-}
{-myStatusBar = spawnPipe . ("gnome-terminal --title " ++ ) . show-}
{-myStatusBarCleanup :: DynamicStatusBarCleanup-}
{-myStatusBarCleanup = safeSpawn "~/.xmonad/print.sh" ["CLEANUP"]-}
dzenCommand (S n) = "dzen2"
                    ++ " -x '0' -y '0'"
                    ++ " -h '16' -w '500'"
                    ++ " -ta 'l'"
                    ++ " -fg '" ++ colorWhiteAlt ++ "'"
                    ++ " -bg '" ++ dzenBg ++ "'"
                    ++ " -fn '" ++ dzenFont ++ "'"
                    ++ " -xs '" ++ show n ++ "'"

bindPPoutput pp h = pp { ppOutput = hPutStrLn h }

main :: IO ()
main = do
  pathHome <- catch (getEnv "HOME") ( const $ return [])
  d        <- catch (getEnv "DISPLAY") ( const $ return [])
  display  <- openDisplay d
  screenCount <- countScreens
  let pathXmonad   = pathHome ++ "/.xmonad"
  let pathIcons    = pathXmonad ++ "/icons"
  let screen       = defaultScreenOfDisplay display
  let screenWidth  = read (show (widthOfScreen screen))  :: Int
  let screenHeight = read (show (heightOfScreen screen)) :: Int
  {-let barTopLeft   = "~/.xmonad/status-bar-launcher.sh"-}
  let barTopLeft   = "dzen2"
                     ++ " -x '0' -y '0'"
                     ++ " -h '16' -w '" ++ show topLeftBarWidth ++ "'"
                     ++ " -ta 'l'"
                     ++ " -fg '" ++ colorWhiteAlt ++ "'"
                     ++ " -bg '" ++ dzenBg ++ "'"
                     ++ " -fn '" ++ dzenFont ++ "'"
  let barTopRight  = "conky -c ~/.xmonad/conkyrc-top-right | ~/.xmonad/layout.sh " ++ show screenWidth ++ " | dzen2"
                     ++ " -x '" ++ show topLeftBarWidth ++ "' -y '0'"
                     ++ " -h '16' -w '" ++ show (screenWidth - topLeftBarWidth) ++ "'"
                     ++ " -ta 'r'"
                     ++ " -fg '" ++ colorWhiteAlt ++ "'"
                     ++ " -bg '" ++ dzenBg ++ "'"
                     ++ " -fn '" ++ dzenFont ++ "'"
  {-dzenTopLeft  <- spawnPipe barTopLeft-}
  dzenTopRight <- spawnPipe barTopRight
  dzens        <- mapM (spawnPipe . dzenCommand) [1 .. screenCount]
  xmonad $ gnomeConfig
    { modMask            = mod4Mask         -- changes the mode key to "super"
    , focusedBorderColor = solarizedOrange  -- color of focused border
    , normalBorderColor  = solarizedBase03  -- color of inactive border
    , borderWidth        = 1                -- width of border around windows
    , terminal           = "gnome-terminal" -- default terminal program
    , workspaces         = myWorkspaces
    , manageHook = manageHook gnomeConfig <+> myManageHook
    {-, logHook = logHookTopLeft dzenTopLeft pathIcons-}
    , logHook = (mapM_ dynamicLogWithPP $ zipWith pp dzens [1 .. screenCount])
                >> updatePointer (Relative 0.5 0.5)
    {-, logHook = updatePointer (Relative 1 1)-}
                {->> mapM_ (bindPPoutput logHookTopLeft) dzens-}
    , layoutHook = mcfLayouts-- $ layoutHook gnomeConfig
    , handleEventHook    = myHandleEventHook
    {-, startupHook = spawn "sh ~/.xmonad/print.sh startup"-}
    {-, startupHook = dynStatusBarStartup myStatusBar myStatusBarCleanup-}
    {-, startupHook        = startTimer 0.5 >>= XS.put . TID-}
    }
    `additionalKeysP`
    [ ("M-<F12>", spawn "gnome-session-quit --logout --no-prompt")
    , ("M1-<F10>", spawn "gnome-screensaver-command -l")
    , ("M1-<F11>", spawn "pm-hybernate")
    , ("M1-<F12>", spawn "pm-suspend")
    {-, ("<XF86Forward>", nextWS)-}
    {-, ("<XF86Back>", prevWS)-}
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-a", sendMessage NextLayout)
    , ("M1-<Tab>", windows W.focusDown)
    , ("M-o", windows W.focusDown)
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
    , ("M-q", spawn "killall conky dzen2" <+> restart "xmonad" True)                           --Restart xmonad
    , ("M-b", spawn "chromium-browser")
    , ("M-`", scratchTerminal)
    ]
    where
      scratchTerminal = namedScratchpadAction myScratchPads "terminal"

-- }}}
-- Status bars ------------------------------------------------------------- {{{
-- Helper functions -------------------------------------------------------- {{{
myIconPath = "/home/sergey/.xmonad/icons/"
wrapTextBox :: String -> String -> String -> String -> String
wrapTextBox fg bg1 bg2 t = "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath  ++ "boxleft.xbm)^ib(1)^r(" ++ show xRes ++ "x" ++ show panelBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg(" ++ fg ++ ")" ++ t ++ "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath ++ "boxright.xbm)^fg(" ++ bg2 ++ ")^r(" ++ show xRes ++ "x" ++ show panelBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg()^ib(0)"

xdoMod :: String -> String
xdoMod key = "/usr/bin/xdotool key alt+" ++ key

wrapClickLayout x = "^ca(1," ++ xdoMod "a" ++ ")" ++ x ++ "^ca()"
wrapClickWorkspace ws = "^ca(1," ++ xdoMod "w;" ++ xdoMod index ++ ")" ++ "^ca(3," ++ xdoMod "w;" ++ xdoMod index ++ ")" ++ ws ++ "^ca()^ca()"
  where
    wsIdxToString Nothing = "1"
    wsIdxToString (Just n) = show $ mod (n+1) $ length myWorkspaces
    index = wsIdxToString (elemIndex ws myWorkspaces)

wrapLoggerBox :: String -> String -> String -> Logger -> Logger
wrapLoggerBox fg bg1 bg2 l = do
  log <- l
  let text = do
      logStr <- log
      return $ wrapTextBox fg bg1 bg2 logStr
  return text
-- }}}
-- Top left (XMonad status) ------------------------------------------------ {{{
pp h s = defaultPP
{-logHookTopLeft h = dynamicLogWithPP $ defaultPP-}
  { ppOutput          = hPutStrLn h
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP) --hide "NSP" from workspace list
  , ppOrder           = \(ws:l:t:x) -> [ws, l, t] ++ x
  , ppSep             = " "
  , ppWsSep           = ""
  , ppCurrent         = wrapTextBox dzenFgDark dzenFgLight dzenBg
  , ppUrgent          = wrapTextBox dzenUrgent dzenBg dzenBg . wrapClickWorkspace
  , ppVisible         = wrapTextBox dzenFgLight  dzenFgDark dzenBg . wrapClickWorkspace
  , ppHiddenNoWindows = wrapTextBox dzenFgDark dzenBg  dzenBg . wrapClickWorkspace
  , ppHidden          = wrapTextBox dzenFgLight dzenBg  dzenBg . wrapClickWorkspace
  , ppTitle           = (" " ++) . dzenColor dzenFgLight dzenBg . dzenEscape . shorten 80
  , ppLayout          = wrapClickLayout . dzenColor dzenFgDark dzenBg .
    (\x -> case x of
    "MouseResizableTile"        -> "^i(" ++ icons ++ "/tall.xbm)"
    "Mirror MouseResizableTile" -> "^i(" ++ icons ++ "/mtall.xbm)"
    "Grid"                      -> "^i(" ++ icons ++ "/grid.xbm)"
    "Full"                      -> "^i(" ++ icons ++ "/full.xbm)"
    _ -> x
    )
  }
  where
    icons = "/home/sergey/.xmonad/icons"

-- }}}
-- Top right (date and time) ----------------------------------------------- {{{

logHookTopRight :: Handle -> X ()
logHookTopRight h = dynamicLogWithPP $ defaultPP
  { ppOutput          = hPutStrLn h
  , ppOrder           = \(_:_:_:x) -> x
  , ppSep             = " "
  , ppExtras          = [ date $ (wrapTextBox colorBlack colorWhiteAlt colorBlack "%A") ++ (wrapTextBox colorWhiteAlt colorGrayAlt colorBlack $ "%Y^fg(" ++ colorGray ++ ").^fg()%m^fg(" ++ colorGray ++ ").^fg()^fg(" ++ colorBlue ++ ")%d^fg() ^fg(" ++ colorGray ++ ")-^fg() %H^fg(" ++ colorGray ++"):^fg()%M^fg(" ++ colorGray ++ "):^fg()^fg(" ++ colorGreen ++ ")%S^fg()") ]
  }

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
