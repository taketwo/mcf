--- Large sections of this file come from
--- github.com/davidbrewer/xmonad-ubuntu-conf

-- Language
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses,  ImplicitParams, PatternGuards #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


import XMonad hiding ( (|||) )
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Layout.Magnifier as Mag
import XMonad.Layout.LimitWindows
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.IndependentScreens

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
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.ShowText
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Commands
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
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
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Util.Timer
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import qualified Data.Map as M
import Data.Ratio ((%))
import Data.List
import Data.IORef
import Data.Monoid
import Data.Maybe (fromJust)
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
import System.IO (Handle, hPutStrLn)

import Solarized
import MCF.Apps
import MCF.Paths
import MCF.Icons
import MCF.Xmobar

unspawn :: String -> X ()
unspawn p = spawn $ "for pid in $(pgrep -f " ++ p ++ "); do kill -9 $pid; done"

-- Appearance -------------------------------------------------------------- {{{

myFont               = "Fantasque Sans Mono"
fontXP               = "xft:" ++ myFont ++ ":pixelsize=14:bold"
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
topLeftBarWidth      = 800
topBarTitleLength    = 80

myTabConfig = defaultTheme
  { activeColor         = colorBorderActive
  , activeTextColor     = colorFgLight
  , activeBorderColor   = colorBorderActive
  , inactiveColor       = colorBg
  , inactiveTextColor   = colorFgDark
  , inactiveBorderColor = colorBg
  , fontName            = fontXP
  }

-- }}}
-- Layouts ----------------------------------------------------------------- {{{

defaultLayouts = avoidStruts $
      gap (windowNavigation mouseResizableTile { draggerType = dragger })
  ||| gap (windowNavigation mouseResizableTile { draggerType = dragger, isMirrored  = True })
  ||| spacing space (windowNavigation Grid)
  ||| gap (tabbedBottom shrinkText myTabConfig)
  {-||| myCode-}
  where
    space = 10
    dragger = FixedDragger (space * 2) (space * 2 - 5)
    gap = gaps [(U, space), (D, space), (L, space), (R, space)]

myCode = named "code" (windowNavigation $ limitWindows 3 $ Mag.magnifiercz' 1.4 $ mouseResizableTile { draggerType = BordersDragger })
myIM = smartBorders $ avoidStruts $ withIM (1 % 5) skype Full
  where
    skype = And (ClassName "Skype") (Role "")
myRViz = named "rviz" (smartBorders $ avoidStruts $ reflectHoriz $ withIM (2 % 3) (ClassName "Rviz") (tabbed shrinkText myTabConfig))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayoutHook =
-- start all workspaces in my home directory, with the ability
-- to switch to a new working dir
  onWorkspace "im" (myIM) $
  onWorkspace "rviz" (myRViz) $
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
  , TI "music"    ""                                         (spawnInShell "ncmpcpp")
  , TI "papers"   "Downloads/papers"                         (spawn "firefox" >> spawn "nautilus ~/Downloads/papers")
  , TI "mendeley" ""                                         (spawn "mendeleydesktop")
  , ti "im"       ""
  , TI "mcf"      ".mcf"                                     (spawnShell)
  , TI "pcl"      "~/Workspace/Libraries/pcl"                (spawnShell)
  , TI "opencv"   "~/Workspace/Libraries/opencv"             (spawnShell)
  , TI "ipy"      ""                                         (spawnInShell "ipython --pylab")
  , TI "mp3"      ""                                         (spawn "easytag" >> spawn "nautilus ~/Downloads/Torrents")
  , TI "xmonad"   ".xmonad"                                  (appEdit "/home/sergey/.mcf/.xmonad/xmonad.hs")
  , ti "gimp"     ""
  ]
  where
    -- Make a default topic item that just spawns a shell.
    ti t d = TI t d spawnShell

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

myScratchPads = [ NS "terminal" spawnTerminal findTerminal manageTerminal ]
  where
    spawnTerminal  = appTerminal ++ " --role=scratchpad"
    findTerminal   = (role =? "scratchpad")
      where
        role = stringProperty "WM_WINDOW_ROLE"
    manageTerminal = customFloating $ W.RationalRect l t w h
      where
        h = 0.5             -- height
        w = 0.5             -- width
        t = (1.0 - h) / 2.0 -- centered top/bottom
        l = (1.0 - w) / 2.0 -- centered left/right

-- }}}
-- Key bindings ------------------------------------------------------------ {{{

myKeyBindingsTable = concat $ table

--    key             M-                     M-S-                    M-C-                    M-S-C-
table =
  [ k "<Return>"     launchTerminal          __                      __                      __
  , k "a"            __                      __                      __                      __
  , k "b"            __                      __                      __                      __
  , k "c"            goUp                    swapUp                  expandVertical          moveUp
  , k "d"            __                      __                      __                      __
  , k "e"            __                      __                      __                      __
  , k "f"            __                      tileFloating            __                      resizeFloatingWindow
  , k "g"            __                      __                      __                      __
  , k "h"            goLeft                  swapLeft                shrinkHorizontal        moveLeft
  , k "i"            __                      __                      __                      __
  , k "j"            __                      __                      __                      __
  , k "k"            __                      __                      __                      __
  , k "l"            __                      __                      __                      __
  , k "m"            gotoMaster              swapMaster              __                      toggleMagnifier
  , k "n"            goRight                 swapRight               expandHorizontal        moveRight
  , k "o"            __                      __                      __                      __
  , k "p"            __                      __                      __                      __
  , k "q"            closeWindow             deleteWorkspace         __                      __
  , k "r"            __                      __                      __                      __
  , k "s"            jumpToNextScreen        jumpToPrevScreen        __                      __
  , k "t"            goDown                  swapDown                shrinkVertical          moveDown
  , k "u"            __                      __                      __                      __
  , k "v"            __                      __                      __                      __
  , k "w"            gotoWorkspace           shiftToWorkspace        createWorkspace         shiftAndGoToWorkspace
  , k "x"            __                      __                      renameWorkspace'        deleteWorkspace
  , k "y"            __                      __                      __                      __
  , k "z"            __                      __                      __                      __
  , k "<Backspace>"  closeWindow             __                      __                      deleteWorkspace
  , k "<Space>"      launchKupfer            nextKeyboardLayout      __                      __
  , k "<Tab>"        nextLayout              resetLayout             __                      __
  , k "`"            scratchTerminal         __                      __                      __
  , k "'"            gridSelect              __                      __                      __
  , k "/"            promptSearch            selectSearch            __                      __
  , k "0"            gotoPrevWorkspace       __                      __                      __
  , k "<F5>"         __                      restartXMonad           __                      __
  , k "<F10>"        __                      logout                  __                      __
  , k "<F11>"        __                      reboot                  __                      __
  , k "<F12>"        __                      powerOff                __                      __
  , [bind "M1-" "<Tab>" gotoNextWindow]
  -- Multimedia keys
  , [bind "" "<XF86AudioMute>"         audioMute]
  , [bind "" "<XF86AudioLowerVolume>"  audioLowerVolume]
  , [bind "" "<XF86AudioRaiseVolume>"  audioRaiseVolume]
  , [bind "" "<XF86AudioPlay>"         audioPlay]
  , [bind "" "<XF86AudioStop>"         audioStop]
  , [bind "" "<XF86Launch5>"           (audioRate 1)]
  , [bind "" "<XF86Launch6>"           (audioRate 2)]
  , [bind "" "<XF86Launch7>"           (audioRate 3)]
  , [bind "" "<XF86Launch8>"           (audioRate 4)]
  , [bind "" "<XF86Launch9>"           (audioRate 5)]
  , [bind "" "<XF86MonBrightnessDown>" brightnessDown]
  , [bind "" "<XF86MonBrightnessUp>"   brightnessUp]
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
    launchBrowser           = Unbound "Launch browser"                      (spawn appBrowser)
    launchTerminal          = Unbound "Launch terminal"                     (spawn appTerminal)
    launchKupfer            = Unbound "Launch kupfer"                       (spawn "kupfer")
    launchCalendar          = Unbound "Launch calendar"                     (appBrowse "--app-id=ejjicmeblgpmajnghnpcppodonldlgfn")
    -- Window navigation
    gotoNextWindow          = Unbound "Switch to next window"               (windows W.focusDown)
    gotoMaster              = Unbound "Move focus to the master window"     (windows W.focusMaster)
    goUp                    = Unbound "Switch to window above"              (sendMessage $ Go U)
    goDown                  = Unbound "Switch to window below"              (sendMessage $ Go D)
    goLeft                  = Unbound "Switch to window to the left"        (sendMessage $ Go L)
    goRight                 = Unbound "Switch to window to the right"       (sendMessage $ Go R)
    swapMaster              = Unbound "Swap with the master window"         (windows W.swapMaster)
    swapUp                  = Unbound "Swap with window above"              (sendMessage $ Swap U)
    swapDown                = Unbound "Swap with window below"              (sendMessage $ Swap D)
    swapLeft                = Unbound "Swap with window to the left"        (sendMessage $ Swap L)
    swapRight               = Unbound "Swap with window to the right"       (sendMessage $ Swap R)
    -- Floating window manipulation
    moveUp                  = Unbound "Move floating window up"             (withFocused (keysMoveWindow (0, -10)))
    moveDown                = Unbound "Move floating window down"           (withFocused (keysMoveWindow (0, 10)))
    moveLeft                = Unbound "Move floating window left"           (withFocused (keysMoveWindow (-10, 0)))
    moveRight               = Unbound "Move floating window right"          (withFocused (keysMoveWindow (10, 0)))
    expandVertical          = Unbound "Expand floating window vertically"   (withFocused (keysResizeWindow (0, 10) (0, 1%2)))
    expandHorizontal        = Unbound "Expand floating window horizontally" (withFocused (keysResizeWindow (10, 0) (1%2, 0)))
    shrinkVertical          = Unbound "Shrink floating window vertically"   (withFocused (keysResizeWindow (0, -10) (0, 1%2)))
    shrinkHorizontal        = Unbound "Shrink floating window horizontally" (withFocused (keysResizeWindow (-10, 0) (1%2, 0)))
    -- Layout management
    shrinkMaster            = Unbound "Shrink master window"                (sendMessage Shrink)
    expandMaster            = Unbound "Expand master window"                (sendMessage Expand)
    nextLayout              = Unbound "Switch to next layout"               (sendMessage NextLayout)
    resetLayout             = Unbound "Switch to default layout"            (sendMessage FirstLayout)
    tileFloating            = Unbound "Push into tile"                      (withFocused $ windows . W.sink)
    resizeFloatingWindow    = Unbound "Resize focused floating window"      (withFocused $ FR.mouseResizeWindow)
    toggleMagnifier         = Unbound "Toggle magnifier"                    (sendMessage Mag.Toggle)
    -- Workspace navigation
    gotoPrevWorkspace       = Unbound "Switch to previous workspace"        (toggleWS' ["NSP"])
    gotoWorkspace           = Unbound "Go to named workspace"               (removeIfEmpty (withWorkspace myXPConfigAutoComplete goto))
    shiftToWorkspace        = Unbound "Shift to named workspace"            (removeIfEmpty (withWorkspace myXPConfigAutoComplete sendX))
    shiftAndGoToWorkspace   = Unbound "Shift and go to named workspace"     (removeIfEmpty (withWorkspace myXPConfigAutoComplete takeX))
    nextWorkspace           = Unbound "Go to next workspace"                (removeIfEmpty (DO.moveTo Next HiddenNonEmptyWS))
    prevWorkspace           = Unbound "Go to previous workspace"            (removeIfEmpty (DO.moveTo Prev HiddenNonEmptyWS))
    createWorkspace         = Unbound "Create named workspace"              (selectWorkspace myXPConfig)
    renameWorkspace'        = Unbound "Rename workspace"                    (renameWorkspace myXPConfig)
    deleteWorkspace         = Unbound "Remove workspace"                    (removeWorkspace)
    -- Misc
    scratchTerminal         = Unbound "Open scratch terminal"               (namedScratchpadAction myScratchPads "terminal")
    restartXMonad           = Unbound "Restart XMonad"                      (spawn "killall xmobar" <+> unspawn "gmaild" <+> restart "xmonad" True)
    jumpToNextScreen        = Unbound "Jump to next physical screen"        (onNextNeighbour W.view)
    jumpToPrevScreen        = Unbound "Jump to previous physical screen"    (onPrevNeighbour W.view)
    powerOff                = Unbound "Power off the system"                (spawn "gnome-session-quit --power-off")
    reboot                  = Unbound "Reboot the system"                   (spawn "gnome-session-quit --reboot")
    logout                  = Unbound "Logout"                              (spawn "session-logout")
    promptSearch            = Unbound "Prompt search"                       (submap . mySearchMap $ myPromptSearch)
    selectSearch            = Unbound "Search X selection"                  (submap . mySearchMap $ mySelectSearch)
    closeWindow             = Unbound "Close the focused window"            (kill)
    gridSelect              = Unbound "Open GridSelect"                     (goToSelected gridSelectConfig)
    -- Keyboard control
    nextKeyboardLayout      = Unbound "Next keyboard layout"                (spawn "keyboard -n")
    -- Audio control
    audioMute               = Unbound "Mute audio"                          (spawn "amixer -D pulse set Master toggle")
    audioLowerVolume        = Unbound "Lower audio volume"                  (spawn "amixer -D pulse set Master 5%-")
    audioRaiseVolume        = Unbound "Raise audio volume"                  (spawn "amixer -D pulse set Master 5%+")
    audioPlay               = Unbound "Play/pause audio playback"           (spawn "mpc toggle")
    audioStop               = Unbound "Stop audio playback"                 (spawn "mpc stop")
    audioRate rating        = Unbound "Rate current song"                   (spawn ("mpdrate " ++ show rating))
    -- Brightness control
    brightnessDown          = Unbound "Brightness down"                     (spawn "xbacklight -dec 1")
    brightnessUp            = Unbound "Brightness up"                       (spawn "xbacklight -inc 1")

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
  , position            = Bottom
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
-- Grid Select ------------------------------------------------------------ {{{
myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList [ ((0, xK_Escape), cancel) , ((0, xK_Return), select) , ((0, xK_c), move (0, -1) >> myNavigation), ((0, xK_t), move (0, 1) >> myNavigation), ((0, xK_n), move (1, 0) >> myNavigation), ((0, xK_h), move (-1, 0) >> myNavigation) ]
navDefaultHandler = const myNavigation

gridSelectConfig = defaultGSConfig { gs_navigate = myNavigation }
-- }}}
-- Search ------------------------------------------------------------------ {{{

mySearchMap method = M.fromList $
        [ ((0, xK_g), method google)
        , ((0, xK_w), method wikipedia)
        , ((0, xK_s), method scholar)
        , ((0, xK_m), method maps)
        , ((0, xK_y), method youtube)
        -- custom searches
        , ((0, xK_e), method multitranEnglish)
        , ((0, xK_d), method multitranDeutsch)
        , ((0, xK_c), method cppreference)
        ]

multitranEnglish = searchEngine "multitran (english)" "http://www.multitran.ru/c/m.exe?l1=1&l2=2&s="
multitranDeutsch = searchEngine "multitran (deutsch)" "http://www.multitran.ru/c/m.exe?l1=3&l2=2&s="
cppreference = searchEngine "c++ reference" "http://en.cppreference.com/mwiki/index.php?search="

-- Prompt search: get input from the user via a prompt, then run the search in
-- the browser and automatically switch to the web workspace
myPromptSearch (SearchEngine name site)
  = inputPrompt myXPConfig ("Search " ++ name) ?+ \s ->
      (search appBrowser site s >> viewWeb)

-- Select search: do a search based on the X selection
mySelectSearch eng = selectSearch eng >> viewWeb

-- Switch to the "web" workspace
viewWeb = windows (W.view "web")

-- }}}
-- Main -------------------------------------------------------------------- {{{

main :: IO ()
main = do
  screenCount      <- countScreens
  display          <- openDisplay $ unsafePerformIO $ getEnv "DISPLAY"
  let screen        = defaultScreenOfDisplay display
  let screenWidth   = read (show (widthOfScreen screen))  :: Int
  let screenHeight  = read (show (heightOfScreen screen)) :: Int
  gmaild           <- spawn "gmaild"
  xmobars          <- mapM (spawnPipe . xmobarConfig) [1 .. screenCount]
  xmonad $ withUrgencyHook NoUrgencyHook $ gnomeConfig
    { modMask            = mod4Mask          -- changes the mode key to "super"
    , focusedBorderColor = colorBorderActive -- color of focused border
    , normalBorderColor  = colorBg           -- color of inactive border
    , borderWidth        = 2                 -- width of border around windows
    , terminal           = appTerminal        -- default terminal program
    , workspaces         = myTopicNames
    , manageHook         = manageHook gnomeConfig <+> myManageHook
    , logHook            = do
        mapM_ dynamicLogWithPP $ zipWith logHookXmobar xmobars [1 .. screenCount]
        updatePointer (0.5, 0.5) (0, 0)
        logHook gnomeConfig
    , layoutHook         = myLayoutHook
    , handleEventHook    = myHandleEventHook
    , startupHook        = setWMName "LG3D"
    }
    `additionalKeysP`
    myKeyBindingsTable
    `additionalMouseBindings`
    [ ((mod4Mask, button2), (\w -> focus w >> FR.mouseResizeWindow w)) ]

-- }}}
-- Status bar -------------------------------------------------------------- {{{

xdoMod :: String -> String
xdoMod key = "/usr/bin/xdotool key super+" ++ key

addPluses :: String -> String
addPluses = init . go
  where go [] = []
        go xs = let (as, bs) = splitAt 1 xs in as ++ ('+' : go bs)

xdoGotoWorkspace :: String -> String
xdoGotoWorkspace ws = xdoMod "w" ++ " " ++ addPluses ws

clickable :: String -> String
clickable ws = xmobarAction (xdoGotoWorkspace ws) 1 ws

xmobarConfig (S n) = "xmobar " ++ pathXmobar ++ " -x '" ++ show n ++ "'"

myWorkspaceSorter = do
  srt <- fmap (namedScratchpadFilterOutWorkspace.) getSortByXineramaPhysicalRule
  return srt

logHookXmobar handle s = xmobarPP
  { ppOutput           = hPutStrLn handle
  , ppSort             = myWorkspaceSorter
  , ppOrder            = \(ws:l:t:x) -> [ws, l, t] ++ x
  , ppSep              = "   "
  , ppWsSep            = " "
  , ppCurrent          = xmobarColor colorFgLight solarizedOrange . wrap " " " "
  , ppUrgent           = xmobarColor colorFgLight solarizedViolet . wrap " " " " . clickable
  , ppVisible          = xmobarColor colorFgLight "" . wrap " " " " . clickable
  , ppHidden           = clickable
  , ppHiddenNoWindows  = const ""
  , ppTitle            = (" " ++) . xmobarColor colorFgLight "" . shorten topBarTitleLength
  , ppLayout           = xmobarAction (xdoMod "Tab") 1 .
    (\x -> case x of
    "MouseResizableTile"        -> xmobarIcon "tall"
    "Mirror MouseResizableTile" -> xmobarIcon "mtall"
    "Spacing 10 Grid"           -> xmobarIcon "grid"
    "Tabbed Simplest"           -> xmobarIcon "full"
    "Tabbed Bottom Simplest"    -> xmobarIcon "full"
    "code"                      -> xmobarIcon "code"
    "im"                        -> ""
    _ -> x
    )
  }

-- }}}
-- HandleEvent hook -------------------------------------------------------- {{{

myHandleEventHook = ED.fullscreenEventHook <+> docksEventHook

-- }}}
-- Manage hook ------------------------------------------------------------- {{{

myManageHook :: ManageHook
myManageHook = manageWindows <+> manageDocks <+> namedScratchpadManageHook myScratchPads <+> fullscreenManageHook

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
    myIMShifts = ["Skype"
                 , "crx_kbpgddbgniojgndnhlkjbkpknjhppkbk" -- Google+ Hangouts application
                 , "crx_nckgahadagoaajjgafhacjanaoiihapd" -- Google+ Hangouts extension
                 , "Empathy"
                 ]

-- Hint: use `xprop` to figure out window class name

-- }}}
