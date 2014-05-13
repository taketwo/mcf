--- Large sections of this file come from
--- github.com/davidbrewer/xmonad-ubuntu-conf

-- Language
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses,  ImplicitParams, PatternGuards #-}

import XMonad hiding ( (|||) )
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
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
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Magnifier as Mag
import XMonad.Layout.LimitWindows
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutCombinators
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
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.AppLauncher as AL

import XMonad.Util.Timer
import XMonad.Util.Cursor
import XMonad.Util.Loggers
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.ShowText
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Commands
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.FlexibleResize as FR
import XMonad.Operations
import XMonad.Core
import XMonad.Config.Gnome
import XMonad.ManageHook
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
import System.Environment (getEnv)

import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import System.Exit
import System.IO (Handle, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
{-import Control.Exception as E-}
import qualified Control.Exception as Exception
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Util.ExtensibleState as XS
import System.IO (Handle, hPutStrLn)

import Solarized
import MCF.Apps
import MCF.Paths
import MCF.Icons

-- Appearance -------------------------------------------------------------- {{{

myFont               = "Liberation Mono"
fontXP               = "xft:" ++ myFont ++ ":pixelsize=11"
fontDzen             = "-*-" ++ myFont ++ "-*-r-normal-*-11-*-*-*-*-*-*-*"
colorBg              = "#3c3b37"
colorFgLight         = "#d7dbd2"
colorFgDark          = "#7f7d76"
colorBorderActive    = solarizedOrange
dzenUrgent           = "#19b6ee"
colorBlack           = "#020202" --Background (Dzen_BG)
colorBlackAlt        = "#1c1c1c" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#101010" --Gray dark
colorWhite           = "#a9a6af" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorBlue            = "#44aacc"
myNormalBorderColor  = colorBlackAlt
myFocusedBorderColor = colorGray
xRes                 = 166
yRes                 = 768
topBarHeight         = 14
topBarBoxHeight      = 12
topLeftBarWidth      = 600
topBarTitleLength    = 80

myTabConfig = defaultTheme
  { activeColor         = colorBg
  , activeTextColor     = colorFgLight
  , activeBorderColor   = colorBorderActive
  , inactiveColor       = colorBg
  , inactiveTextColor   = colorFgDark
  , inactiveBorderColor = colorBg
  , fontName            = fontXP
  }

-- }}}
-- Layouts ----------------------------------------------------------------- {{{

defaultLayouts = smartBorders $ avoidStruts $
      windowNavigation mouseResizableTile
        { draggerType = BordersDragger }
  ||| windowNavigation mouseResizableTile
        { draggerType = BordersDragger
        , isMirrored  = True }
  ||| windowNavigation Grid
  ||| tabbedBottom shrinkText myTabConfig
  ||| myCode

myCode = named "code" (windowNavigation $ limitWindows 3 $ Mag.magnifiercz' 1.4 $ mouseResizableTile { draggerType = BordersDragger })
myIM = named "im" (smartBorders $ avoidStruts $ withIM (1 % 5) (Title "Contact List") Grid)
myFigures = named "figures" (windowNavigation $ Mag.magnifierOff $ GridRatio (4/3))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayoutHook =
-- start all workspaces in my home directory, with the ability
-- to switch to a new working dir
  workspaceDir "~" $
  onWorkspace "figures" (myFigures) $
  onWorkspace "im" (myIM) $
  {-$-} defaultLayouts

-- }}}
-- Topics ------------------------------------------------------------------ {{{

data TopicItem = TI { topicName   :: Topic
                    , topicDir    :: Dir
                    , topicAction :: X ()
                    }

myTopics :: [TopicItem]
myTopics =
  [ TI "web"      ""                                         (spawn appBrowser)
  , ti "music"    ""
  , TI "papers"   "Downloads/papers"                         (spawn "firefox" >> spawn "nautilus ~/Downloads/papers")
  , TI "mendeley" ""                                         (spawn "mendeleydesktop")
  , ti "im"       ""
  , TI "mcf"      ".mcf"                                     (spawnShell)
  , TI "tocs"     "/media/Workspace/Projects/tocs"           (spawnShell)
  , TI "eval"     "/media/Workspace/Projects/tocs/data"      (spawnShell)
  , TI "blog"     "/media/Workspace/Projects/tocs/blog"      (blog)
  , TI "pcl"      "/media/Workspace/Libraries/pcl-canonical" (spawnShell)
  , TI "ipy"      ""                                         (spawnInShell "ipython --pylab")
  , TI "mp3"      ""                                         (spawn "easytag" >> spawn "nautilus /media/Files/Downloads")
  , TI "xmonad"   ".xmonad"                                  (appEdit "/home/sergey/.mcf/.xmonad/xmonad.hs")
  , ti "figures"  ""
  , ti "gimp"     ""
  ]
  where
    -- Make a default topic item that just spawns a shell.
    ti t d = TI t d spawnShell

blog = do
  appBrowse "--new-window file:///media/Workspace/Projects/tocs/blog/repository/blogweb/_build/html/tocs/alexandrov/tmp.html"
  spawnShell
  spawnShell

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n, d)) myTopics
  , defaultTopicAction = const (return ())
  , defaultTopic = "web"
  , maxTopicHistory = 10
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n, a)) myTopics
  }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ appTerminal ++ " --working-directory " ++ dir

spawnInShell :: String -> X ()
spawnInShell cmd = spawn $ appTerminal ++ " -e '" ++ cmd ++ "'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfigAutoComplete goto

-- }}}
-- Scratchpads ------------------------------------------------------------- {{{

myScratchPads = [ NS "terminal" spawnTerminal  findTerminal  manageTerminal ]
  where
    spawnTerminal  = appTerminal ++ " --disable-factory --name scratchpad --zoom 0.8"
    findTerminal   = resource  =? "scratchpad"
    manageTerminal = customFloating $ W.RationalRect l t w h
      where
        h = 0.5             -- height
        w = 0.5             -- width
        t = (1.0 - h) / 2.0 -- centered top/bottom
        l = (1.0 - w) / 2.0 -- centered left/right

-- }}}
-- Key bindings ------------------------------------------------------------ {{{

myKeyBindingsTable = concat $ table

--        key                  M-              M-S-           M-C-           M-S-C-
table =
  [ k "<Return>"     launchTerminal         __              __                __
  , k "a"            __                     __              __                __
  , k "b"            __               __            launchBrowser        __
  , k "c"            goUp             swapUp        launchCalendar       shrinkMaster
  {-, k "d"            launchWithDmenu      __              __                __-}
  {-, k "e"            wicdNetwork          __              __                __-}
  , k "f"            __               tileFloating        __           resizeFloatingWindow
  {-, k "g"            gotoMenu'        bringMenu'    windowMenu'      xmonadCommands-}
  , k "h"            goLeft           swapLeft            __           shrinkMaster
  , k "i"            __               __              __                __
  , k "j"                __               __              __                __
  {-, k "k"            focusUrgent'         __              __         clearUrgents'-}
  {-, k "l"            expandMaster     shrinkMaster  incMaster        decMaster -}
  , k "m"            gotoMaster       swapMaster          __           toggleMagnifier
  , k "n"            goRight          swapRight           __           expandMaster
  , k "o"                __               __              __                __
  , k "p"                __               __              __                __
  , k "q"            closeWindow          __              __                __
  , k "r"                __               __              __           __
  {-, k "s"            toggleStruts     cntrlCenter         __         swapScreens-}
  , k "s"            swapScreens          __              __                __
  , k "t"            goDown           swapDown            __           expandMaster
  {-, k "u"            gotoScreen0      sendScreen0   takeScreen0      swapScreen0-}
  {-, k "v"            volumeMuteToggle volumeDown    volumeUp                __-}
  , k "w"            gotoWorkspace    shiftToWorkspace createWorkspace    shiftAndGoToWorkspace
  , k "x"            nextWorkspace    prevWorkspace renameWorkspace' deleteWorkspace
  , k "y"                __               __              __                __
  , k "z"                __               __              __                __
  , k "<Backspace>"  closeWindow          __              __         deleteWorkspace
  , k "<Space>"      launchKupfer           __              __                __
  , k "<Tab>"        nextLayout       resetLayout         __                __
  {-, k "-"            gotoRecentWS     sendRecentWS  takeRecentWS            __-}
  , k "`"            scratchTerminal      __              __                __
  , k "/"            promptSearch     selectSearch             __                __
  , k "0"            gotoPrevWorkspace      __              __                __
  , k "<F5>"             __           restartXMonad       __                __
  , k "<F10>"            __           logout              __                __
  , k "<F11>"            __           reboot              __                __
  , k "<F12>"            __           powerOff            __                __
  , k "<Esc>"        nextKeyboardLayout    __           __                __
  , [bind "M1-" "<Tab>" gotoNextWindow]
  -- Multimedia keys
  , [bind "" "<XF86AudioMute>"        audioMute]
  , [bind "" "<XF86AudioLowerVolume>" audioLowerVolume]
  , [bind "" "<XF86AudioRaiseVolume>" audioRaiseVolume]
  , [bind "" "<XF86AudioPlay>"        audioPlay]
  , [bind "" "<XF86AudioStop>"        audioStop]
  , [bind "" "<XF86Launch5>"          audioRate1]
  , [bind "" "<XF86Launch6>"          audioRate2]
  , [bind "" "<XF86Launch7>"          audioRate3]
  , [bind "" "<XF86Launch8>"          audioRate4]
  , [bind "" "<XF86Launch9>"          audioRate5]
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

    -- Actions
    -- Launch program
    launchBrowser           = Unbound "Launch browser"                  (spawn appBrowser)
    launchTerminal          = Unbound "Launch terminal"                 (spawn appTerminal)
    launchKupfer            = Unbound "Launch kupfer"                   (spawn "kupfer")
    launchCalendar          = Unbound "Launch calendar"                 (appBrowse "--app-id=ejjicmeblgpmajnghnpcppodonldlgfn")
    -- Window navigation
    gotoNextWindow          = Unbound "Switch to next window"           (windows W.focusDown)
    gotoMaster              = Unbound "Move focus to the master window" (windows W.focusMaster)
    goUp                    = Unbound "Switch to window above"          (sendMessage $ Go U)
    goDown                  = Unbound "Switch to window below"          (sendMessage $ Go D)
    goLeft                  = Unbound "Switch to window to the left"    (sendMessage $ Go L)
    goRight                 = Unbound "Switch to window to the right"   (sendMessage $ Go R)
    swapMaster              = Unbound "Swap with the master window"     (windows W.swapMaster)
    swapUp                  = Unbound "Swap with window above"          (sendMessage $ Swap U)
    swapDown                = Unbound "Swap with window below"          (sendMessage $ Swap D)
    swapLeft                = Unbound "Swap with window to the left"    (sendMessage $ Swap L)
    swapRight               = Unbound "Swap with window to the right"   (sendMessage $ Swap R)
    -- Layout management
    shrinkMaster            = Unbound "Shrink master window"            (sendMessage Shrink)
    expandMaster            = Unbound "Expand master window"            (sendMessage Expand)
    nextLayout              = Unbound "Switch to next layout"           (sendMessage NextLayout)
    resetLayout             = Unbound "Switch to default layout"        (sendMessage FirstLayout)
    tileFloating            = Unbound "Push into tile"                  (withFocused $ windows . W.sink)
    resizeFloatingWindow    = Unbound "Resize focused floating window"  (withFocused $ FR.mouseResizeWindow)
    toggleMagnifier         = Unbound "Toggle magnifier"                (sendMessage Mag.Toggle)
    -- Workspace navigation
    gotoPrevWorkspace       = Unbound "Switch to previous workspace"    (toggleWS' ["NSP"])
    gotoWorkspace           = Unbound "Go to named workspace"           (removeIfEmpty (withWorkspace myXPConfigAutoComplete goto))
    shiftToWorkspace        = Unbound "Shift to named workspace"        (removeIfEmpty (withWorkspace myXPConfigAutoComplete sendX))
    shiftAndGoToWorkspace   = Unbound "Shift and go to named workspace" (removeIfEmpty (withWorkspace myXPConfigAutoComplete takeX))
    nextWorkspace           = Unbound "Go to next workspace"            (removeIfEmpty (DO.moveTo Next HiddenNonEmptyWS))
    prevWorkspace           = Unbound "Go to previous workspace"        (removeIfEmpty (DO.moveTo Prev HiddenNonEmptyWS))
    createWorkspace         = Unbound "Create named workspace"          (selectWorkspace myXPConfig)
    renameWorkspace'        = Unbound "Rename workspace"                (renameWorkspace myXPConfig)
    deleteWorkspace         = Unbound "Remove workspace"                (removeWorkspace)
    -- Misc
    scratchTerminal         = Unbound "Open scratch terminal"           (namedScratchpadAction myScratchPads "terminal")
    restartXMonad           = Unbound "Restart XMonad"                  (spawn "killall conky dzen2" <+> restart "xmonad" True)
    swapScreens             = Unbound "Swap current and next screen"    (nextScreen)
    powerOff                = Unbound "Power off the system"            (spawn "gnome-session-quit --power-off")
    reboot                  = Unbound "Reboot the system"               (spawn "gnome-session-quit --reboot")
    logout                  = Unbound "Logout"                          (spawn "gnome-session-quit --no-prompt")
    nextKeyboardLayout      = Unbound "Switch next keyboard layout"     (spawn "keyboard -n")
    promptSearch            = Unbound "Prompt search"                   (submap . mySearchMap $ myPromptSearch)
    selectSearch            = Unbound "Search X selection"              (submap . mySearchMap $ mySelectSearch)
    closeWindow             = Unbound "Close the focused window"        (kill)
    -- Audio control
    audioMute               = Unbound "Mute audio"                      (spawn "amixer -D pulse set Master toggle")
    audioLowerVolume        = Unbound "Lower audio volume"              (spawn "amixer set Master 5%-")
    audioRaiseVolume        = Unbound "Raise audio volume"              (spawn "amixer set Master 5%+")
    audioPlay               = Unbound "Play/pause audio playback"       (spawn "rhythmbox-client --play-pause")
    audioStop               = Unbound "Stop audio playback"             (spawn "rhythmbox-client --pause")
    audioRate1              = Unbound "Rate current song with 1 star"   (spawn "rhythmbox-client --set-rating 1")
    audioRate2              = Unbound "Rate current song with 2 star"   (spawn "rhythmbox-client --set-rating 2")
    audioRate3              = Unbound "Rate current song with 3 star"   (spawn "rhythmbox-client --set-rating 3")
    audioRate4              = Unbound "Rate current song with 4 star"   (spawn "rhythmbox-client --set-rating 4")
    audioRate5              = Unbound "Rate current song with 5 star"   (spawn "rhythmbox-client --set-rating 5")

    {-gotoRecentWS     = Unbound "Switch to the most recently visited invisible workspace" (windows gotoRecent)-}
    {-sendRecentWS     = Unbound   "Send to the most recently visited invisible workspace" (windows sendRecent)-}
    {-takeRecentWS     = Unbound   "Take to the most recently visited invisible workspace" (windows takeRecent)-}

-- Two varieties of Action: B(ound) is aware of the key that was used to
-- invoke it, U(nbound) is not aware of the key.
data Action = Unbound String (          X ()) |
              Bound   String (String -> X ())


myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font                = fontXP
  , bgColor             = colorBg
  , fgColor             = colorFgLight
  , bgHLight            = colorBlue
  , fgHLight            = colorBlack
  , borderColor         = colorGrayAlt
  , promptBorderWidth   = 0
  , height              = topBarHeight
  , position            = Top
  , historySize         = 100
  , historyFilter       = deleteConsecutive
  , autoComplete        = Nothing
  }

myXPConfigAutoComplete :: XPConfig
myXPConfigAutoComplete = myXPConfig
 -- If only one completion remains, auto-select it after 1
 -- microsecond. Increasing the delay could help to stop accidentally
 -- sending keypresses to the newly focused window, but with my
 -- current usage, 1 microsecond is working just fine.
  { autoComplete = Just 1 }

gotoX = windows . W.view
sendX = windows . W.shift
takeX = sendX ->> gotoX

removeIfEmpty = removeEmptyWorkspaceAfterExcept myTopicNames

-- Helpers for performing multiple actions on the same entity
infixl 1 ->>
(a ->> b) c = do a c
                 b c

-- }}}
-- Search ------------------------------------------------------------------ {{{

mySearchMap method = M.fromList $
        [ ((0, xK_g), method google)
        , ((0, xK_w), method wikipedia)
        , ((0, xK_s), method scholar)
        , ((0, xK_m), method maps)
        , ((0, xK_a), method alpha)
        , ((0, xK_l), method lucky)
        -- custom searches
        , ((0, xK_e), method multitranEnglish)
        , ((0, xK_d), method multitranDeutsch)
        ]

multitranEnglish = searchEngine "multitranEnglish" "http://www.multitran.ru/c/m.exe?l1=1&l2=2&s="
multitranDeutsch = searchEngine "multitranDeutsch" "http://www.multitran.ru/c/m.exe?l1=3&l2=2&s="

-- Prompt search: get input from the user via a prompt, then run the search in
-- the browser and automatically switch to the web workspace
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->
      (search appBrowser site s >> viewWeb)

-- Select search: do a search based on the X selection
mySelectSearch eng = selectSearch eng >> viewWeb

-- Switch to the "web" workspace
viewWeb = windows (W.view "web")

-- }}}
-- Main -------------------------------------------------------------------- {{{

main :: IO ()
main = do
  screenCount     <- countScreens
  display         <- openDisplay $ unsafePerformIO $ getEnv "DISPLAY"
  let screen       = defaultScreenOfDisplay display
  let screenWidth  = read (show (widthOfScreen screen))  :: Int
  let screenHeight = read (show (heightOfScreen screen)) :: Int
  dzenTopRight <- spawnPipe (barTopRight screenWidth screenHeight)
  dzensTopLeft <- mapM (spawnPipe . barTopLeft) [1 .. screenCount]
  xmonad $ withUrgencyHook NoUrgencyHook $ gnomeConfig
    { modMask            = mod4Mask          -- changes the mode key to "super"
    , focusedBorderColor = colorBorderActive -- color of focused border
    , normalBorderColor  = colorBg           -- color of inactive border
    , borderWidth        = 1                 -- width of border around windows
    , terminal           = appTerminal        -- default terminal program
    , workspaces         = myTopicNames
    , manageHook = manageHook gnomeConfig <+> myManageHook
    , logHook = (mapM_ dynamicLogWithPP $ zipWith logHookTopLeft dzensTopLeft [1 .. screenCount])
                >> updatePointer (Relative 0.5 0.5)
    , layoutHook = myLayoutHook
    , handleEventHook    = myHandleEventHook
    , startupHook        = setWMName "LG3D"
    }
    `additionalKeysP`
    myKeyBindingsTable
-- }}}
-- Status bars ------------------------------------------------------------- {{{
-- Helper functions -------------------------------------------------------- {{{
wrapTextBox :: String -> String -> String -> String -> String
wrapTextBox fg bg1 bg2 t = "^fg(" ++ bg1 ++ ")" ++ getIcon "boxleft" ++ "^ib(1)^r(" ++ show xRes ++ "x" ++ show topBarBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg(" ++ fg ++ ")" ++ t ++ "^fg(" ++ bg1 ++ ")" ++ getIcon "boxright" ++ "^fg(" ++ bg2 ++ ")^r(" ++ show xRes ++ "x" ++ show topBarBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg()^ib(0)"

xdoMod :: String -> String
xdoMod key = "/usr/bin/xdotool key super+" ++ key

wrapClickLayout x = "^ca(1," ++ xdoMod "Tab" ++ ")" ++ x ++ "^ca()"

wrapLoggerBox :: String -> String -> String -> Logger -> Logger
wrapLoggerBox fg bg1 bg2 l = do
  log <- l
  let text = do
      logStr <- log
      return $ wrapTextBox fg bg1 bg2 logStr
  return text
-- }}}
-- Top left (XMonad status) ------------------------------------------------ {{{

barTopLeft (S n) = "dzen2"
                   ++ " -x '0' -y '0'"
                   ++ " -h '" ++ show topBarHeight ++ "' -w '" ++ show topLeftBarWidth ++ "'"
                   ++ " -ta 'l'"
                   ++ " -fg '" ++ colorWhiteAlt ++ "'"
                   ++ " -bg '" ++ colorBg ++ "'"
                   ++ " -fn '" ++ fontDzen ++ "'"
                   ++ " -xs '" ++ show n ++ "'"

logHookTopLeft handle s = defaultPP
  { ppOutput          = hPutStrLn handle
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
  , ppOrder           = \(ws:l:t:x) -> [ws, l, t] ++ x
  , ppSep             = " "
  , ppWsSep           = ""
  , ppCurrent         =                      wrapTextBox colorFgDark  colorFgLight colorBg
  , ppUrgent          =                      wrapTextBox solarizedBase3 solarizedRed colorBg
  , ppVisible         =                      wrapTextBox colorFgLight colorFgDark  colorBg
  , ppHiddenNoWindows = const ""
  , ppHidden          =                      wrapTextBox colorFgLight colorBg      colorBg
  , ppTitle           = (" " ++)           . dzenColor   colorFgLight colorBg             . dzenEscape . shorten topBarTitleLength
  , ppLayout          = wrapClickLayout    . dzenColor   colorFgDark  colorBg             .
    (\x -> case x of
    "MouseResizableTile"        -> getIcon "tall"
    "Mirror MouseResizableTile" -> getIcon "mtall"
    "Grid"                      -> getIcon "grid"
    "Tabbed Bottom Simplest"    -> getIcon "full"
    "code"                      -> getIcon "code"
    "im"                        -> ""
    "figures"                   -> ""
    _ -> x
    )
  }

-- }}}
-- Top right (System status) ----------------------------------------------- {{{

barTopRight :: Int -> Int -> String
barTopRight screenWidth screenHeight = "conky -c ~/.xmonad/conkyrc-top-right | dzen2"
                     ++ " -x '" ++ show topLeftBarWidth ++ "' -y '0'"
                     ++ " -h '" ++ show topBarHeight ++ "' -w '" ++ show (screenWidth - topLeftBarWidth) ++ "'"
                     ++ " -ta 'r'"
                     ++ " -fg '" ++ colorWhiteAlt ++ "'"
                     ++ " -bg '" ++ colorBg ++ "'"
                     ++ " -fn '" ++ fontDzen ++ "'"

-- }}}
-- }}}
-- HandleEvent hook -------------------------------------------------------- {{{

myHandleEventHook = ED.fullscreenEventHook <+> docksEventHook

-- }}}
-- Manage hook ------------------------------------------------------------- {{{

myManageHook :: ManageHook
myManageHook = manageWindows <+> manageDocks <+> namedScratchpadManageHook myScratchPads

manageWindows = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doFloat   | c <- myCFloats]
    , [className =? u --> doUnfloat | u <- myCUnFloats] -- does not work for Gimp actually
    , [title     =? t --> doFloat   | t <- myTFloats]
    , [resource  =? r --> doFloat   | r <- myRFloats]
    , [resource  =? i --> doIgnore  | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "gimp" | x <- myGimpShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "mendeley" | x <- myMendeleyShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "im" | x <- myIMShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "music" | x <- myMusicShifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    doUnfloat = ask >>= doF . W.sink
    myCFloats = ["Exe"]
    myCUnFloats = ["Gimp", "Nautilus"]
    myTFloats = ["Downloads", "Save As...", "Export Image"]
    myRFloats = []
    myIgnores = []
    myGimpShifts = ["Gimp"]
    myMendeleyShifts = ["Mendeley Desktop"]
    myMusicShifts = ["Rhythmbox", "Workrave"]
    myIMShifts = ["Skype"
                 , "crx_kbpgddbgniojgndnhlkjbkpknjhppkbk" -- Google+ Hangouts application
                 , "crx_nckgahadagoaajjgafhacjanaoiihapd" -- Google+ Hangouts extension
                 , "Empathy"
                 ]

-- Hint: use `xprop` to figure out window class name

-- }}}
