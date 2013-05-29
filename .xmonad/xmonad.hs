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
import XMonad.Actions.Commands
import XMonad
import XMonad.Operations
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

dzenFont             = "-*-Liberation Mono-*-r-normal-*-11-*-*-*-*-*-*-*"
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
topBarHeight         = 14
topBarBoxHeight      = 12
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
-- Key bindings ------------------------------------------------------------ {{{

myKeyBindingsTable = concat $ table

--        key                  M-              M-S-           M-C-           M-S-C-
table =
  [ k "<Return>"     openTerminal         __              __                __
  {-, k "a"            gotoScreen1      sendScreen1   takeScreen1      swapScreen1-}
  {-, k "b"            gotoWorkspace    sendWorkspace takeWorkspace    makeWorkspace-}
  , k "b"            __    __ openBrowser    __
  , k "c"            goUp             swapUp        openBrowser        shrinkMaster
  {-, k "d"            launchWithDmenu      __              __                __-}
  {-, k "e"            wicdNetwork          __              __                __-}
  , k "f"            __               tileFloating        __                __
  {-, k "g"            gotoMenu'        bringMenu'    windowMenu'      xmonadCommands-}
  , k "h"            goLeft           swapLeft            __           shrinkMaster
  , k "i"                __               __              __                __
  , k "j"                __               __              __                __
  {-, k "k"            focusUrgent'         __              __         clearUrgents'-}
  {-, k "l"            expandMaster     shrinkMaster  incMaster        decMaster -}
  {-, k "m"            gotoMaster           __        shiftMaster'            __-}
  {-, k "n"            nextWindow       prevWindow    nextWindowSwap   prevWindowSwap-}
  , k "n"            goRight          swapRight           __           expandMaster
  , k "o"                __               __              __                __
  {-, k "p"            prevWindow       nextWindow    prevWindowSwap   nextWindowSwap -- reversed version of 'n'-}
  , k "q"            restartXMonad        __              __                __
  , k "r"                __               __        openRhythmbox           __
  {-, k "s"            toggleStruts     cntrlCenter         __         swapScreens-}
  , k "s"            swapScreens          __              __                __
  , k "t"            goDown           swapDown            __           expandMaster
  {-, k "u"            gotoScreen0      sendScreen0   takeScreen0      swapScreen0-}
  {-, k "v"            volumeMuteToggle volumeDown    volumeUp                __-}
  {-, k "w"            nextWorkspace    prevWorkspace renameWorkspace' deleteWorkspace-}
  , k "w"            closeWindow    __ __ __
  , k "x"                __               __              __                __
  , k "y"                __               __              __                __
  , k "z"                __               __              __                __
  {-, k "<Backspace>"  closeWindow          __              __         deleteWorkspace-}
  , k "<Space>"          __               __       openKupfer               __
  , k "<Tab>"        nextLayout       resetLayout         __                __
  {-, k "-"            gotoRecentWS     sendRecentWS  takeRecentWS            __-}
  , k "`"            scratchTerminal      __              __                __
  , k "0"            toggleWorkspace      __              __                __
  , k "<F10>"            __           logout              __                __
  , k "<F11>"            __           reboot              __                __
  , k "<F12>"            __           powerOff            __                __
  , k "<Esc>"        nextKeyboardLayout    __           __                __
  , [bind "M1-" "<Tab>" nextWindow]
  ]
  where
    k key m ms mc msc =
      [ bind "M-"      key m
      , bind "M-S-"    key ms
      , bind "M-C-"    key mc
      , bind "M-S-C-"  key msc
      ]
    bind modifiers key (Unbound comment action) = (modifiers ++ key, action)
    bind modifiers key (Bound comment action) = (modifiers ++ key, action $ modifiers ++ key)
    __ = Bound "Available for use"
         (\key -> spawn $ "xmessage '" ++ key ++ " is not bound.'")
    openTerminal     = Unbound "Open terminal"             (spawn myTerminal)
    scratchTerminal  = Unbound "Open scratch terminal"     (namedScratchpadAction myScratchPads "terminal")
    openBrowser      = Unbound "Open web browser"          (spawn "chromium-browser")
    openRhythmbox    = Unbound "Open Rhythmbox"            (spawn "rhythmbox")
    openKupfer       = Unbound "Open Kupfer"               (spawn "kupfer")
    closeWindow      = Unbound "Close the focused window"  (kill)
    toggleWorkspace  = Unbound "Switch to previous workspace" (toggleWS' ["NSP"])
    swapUp           = Unbound "Swap with window above"    (sendMessage $ Swap U)
    swapDown         = Unbound "Swap with window below"    (sendMessage $ Swap D)
    swapLeft         = Unbound "Swap with window to the left"  (sendMessage $ Swap L)
    swapRight        = Unbound "Swap with window to the right" (sendMessage $ Swap R)
    goUp             = Unbound "Switch to window above"    (sendMessage $ Go U)
    goDown           = Unbound "Switch to window below"    (sendMessage $ Go D)
    goLeft           = Unbound "Switch to window to the left"  (sendMessage $ Go L)
    goRight          = Unbound "Switch to window to the right" (sendMessage $ Go R)
    shrinkMaster     = Unbound "Shrink master window" (sendMessage Shrink)
    expandMaster     = Unbound "Expand master window" (sendMessage Expand)
    nextLayout       = Unbound "Switch to next layout"     (sendMessage NextLayout)
    nextWindow       = Unbound "Switch to next window" (windows W.focusDown)
    tileFloating     = Unbound "Push into tile" (withFocused $ windows . W.sink)
    resetLayout      = Unbound "Switch to default layout"  (sendMessage FirstLayout)
    restartXMonad    = Unbound "Restart XMonad"                 (spawn "killall conky dzen2" <+> restart "xmonad" True)
    swapScreens      = Unbound "Swap current and next screen"   (nextScreen)
    powerOff         = Unbound "Power off the system"   (spawn "gnome-session-quit --power-off")
    reboot           = Unbound "Reboot the system"      (spawn "gnome-session-quit --reboot")
    logout           = Unbound "Logout"                 (spawn "gnome-session-quit --no-prompt")
    nextKeyboardLayout = Unbound "Switch next keyboard layout" (spawn "keyboard -n")

-- Two varieties of Action: B(ound) is aware of the key that was used to
-- invoke it, U(nbound) is not aware of the key.
data Action = Unbound String (          X ()) |
              Bound   String (String -> X ())

-- }}}
-- Main -------------------------------------------------------------------- {{{

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
  let barTopRight  = "conky -c ~/.xmonad/conkyrc-top-right | dzen2"
                     ++ " -x '" ++ show topLeftBarWidth ++ "' -y '0'"
                     ++ " -h '" ++ show topBarHeight ++ "' -w '" ++ show (screenWidth - topLeftBarWidth) ++ "'"
                     ++ " -ta 'r'"
                     ++ " -fg '" ++ colorWhiteAlt ++ "'"
                     ++ " -bg '" ++ dzenBg ++ "'"
                     ++ " -fn '" ++ dzenFont ++ "'"
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
    , logHook = (mapM_ dynamicLogWithPP $ zipWith (logHookTopLeft pathIcons) dzens [1 .. screenCount])
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
    myKeyBindingsTable
    {-[ ("M-<F12>", spawn "gnome-session-quit --logout --no-prompt")-}
    {-, ("M1-<F10>", spawn "gnome-screensaver-command -l")-}
    {-, ("M1-<F11>", spawn "pm-hybernate")-}
    {-, ("M1-<F12>", spawn "pm-suspend")-}
    {-, ("M-S-<Space>", windows W.swapMaster)-}
    {-, ("M-S-w", kill)-}
    {-, ("M1-<F4>", kill)-}

-- }}}
-- Status bars ------------------------------------------------------------- {{{
-- Helper functions -------------------------------------------------------- {{{
myIconPath = "/home/sergey/.xmonad/icons/"
wrapTextBox :: String -> String -> String -> String -> String
wrapTextBox fg bg1 bg2 t = "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath  ++ "boxleft.xbm)^ib(1)^r(" ++ show xRes ++ "x" ++ show topBarBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg(" ++ fg ++ ")" ++ t ++ "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath ++ "boxright.xbm)^fg(" ++ bg2 ++ ")^r(" ++ show xRes ++ "x" ++ show topBarBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg()^ib(0)"

xdoMod :: String -> String
xdoMod key = "/usr/bin/xdotool key super+" ++ key

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

dzenCommand (S n) = "dzen2"
                    ++ " -x '0' -y '0'"
                    ++ " -h '" ++ show topBarHeight ++ "' -w '" ++ show topLeftBarWidth ++ "'"
                    ++ " -ta 'l'"
                    ++ " -fg '" ++ colorWhiteAlt ++ "'"
                    ++ " -bg '" ++ dzenBg ++ "'"
                    ++ " -fn '" ++ dzenFont ++ "'"
                    ++ " -xs '" ++ show n ++ "'"

logHookTopLeft icons handle s = defaultPP
  { ppOutput          = hPutStrLn handle
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP) -- hide "NSP" from workspace list
  , ppOrder           = \(ws:l:t:x) -> [ws, l, t] ++ x
  , ppSep             = " "
  , ppWsSep           = ""
  , ppCurrent         =                      wrapTextBox dzenFgDark  dzenFgLight dzenBg
  , ppUrgent          =                      wrapTextBox dzenUrgent  dzenBg      dzenBg . wrapClickWorkspace
  , ppVisible         =                      wrapTextBox dzenFgLight dzenFgDark  dzenBg . wrapClickWorkspace
  , ppHiddenNoWindows =                      wrapTextBox dzenFgDark  dzenBg      dzenBg . wrapClickWorkspace
  , ppHidden          =                      wrapTextBox dzenFgLight dzenBg      dzenBg . wrapClickWorkspace
  , ppTitle           = (" " ++)           . dzenColor   dzenFgLight dzenBg             . dzenEscape . shorten 80
  , ppLayout          = wrapClickLayout    . dzenColor   dzenFgDark  dzenBg             .
    (\x -> case x of
    "MouseResizableTile"        -> "^i(" ++ icons ++ "/tall.xbm)"
    "Mirror MouseResizableTile" -> "^i(" ++ icons ++ "/mtall.xbm)"
    "Grid"                      -> "^i(" ++ icons ++ "/grid.xbm)"
    "Full"                      -> "^i(" ++ icons ++ "/full.xbm)"
    _ -> x
    )
  }

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
