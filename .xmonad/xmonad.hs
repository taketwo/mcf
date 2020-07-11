{-# LANGUAGE ImplicitParams, FlexibleContexts #-}

import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FloatSnap

import XMonad.Config.Kde
import XMonad.Config.Gnome
import XMonad.Config.Desktop

import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier as Mag
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ComboP
import XMonad.Layout.Column
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.FlexibleResize as FR
import qualified XMonad.Hooks.EwmhDesktops as ED
import qualified XMonad.StackSet as W

import Control.Monad (liftM2)
import Data.Ratio ((%))
import System.Posix.Env (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (maybe, fromMaybe)
import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import qualified Data.Map as M

import Solarized
import MCF.Apps
import MCF.Paths
import MCF.Polybar

-- Appearance -------------------------------------------------------------- {{{

fontSpec             = "xft:Fantasque Sans Mono:pixelsize=14:bold"
colorBg              = "#3c3b37"
colorFgPrimary       = "#c7c2bb"
colorFgSecondary     = "#6c6b65"
colorAccent          = "#ffffff"
colorBorderActive    = solarizedOrange
barHeight            = 22

myTabConfig = defaultTheme
  { activeColor         = colorBorderActive
  , activeTextColor     = colorAccent
  , activeBorderColor   = colorBorderActive
  , inactiveColor       = colorBg
  , inactiveTextColor   = colorFgSecondary
  , inactiveBorderColor = colorBg
  , fontName            = fontSpec
  }

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font                = fontSpec
  , bgColor             = colorBg
  , fgColor             = colorFgPrimary
  , promptBorderWidth   = 0
  , height              = barHeight
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

-- }}}
-- Layouts ----------------------------------------------------------------- {{{

defaultLayouts = fullscreenFull $ smartBorders $ avoidStruts $
      windowNavigation mouseResizableTile { draggerType = BordersDragger }
  ||| windowNavigation mouseResizableTile { draggerType = BordersDragger, isMirrored  = True }
  ||| windowNavigation Grid
  ||| tabbedBottom shrinkText myTabConfig

layoutMsg = fullscreenFull $ named "msg" (combineTwoP (Column 3.0) (tabbed shrinkText myTabConfig) (tabbedBottom shrinkText myTabConfig) (ClassName "Slack"))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayoutHook =
-- start all workspaces in my home directory, with the ability
-- to switch to a new working dir
  onWorkspace "msg" (layoutMsg) $
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
  , TI "papers"   "~/Downloads/papers"                       (spawn "firefox" >> spawn "nautilus ~/Downloads/papers")
  , TI "mendeley" ""                                         (spawn "mendeleydesktop")
  , TI "zeal"     ""                                         (spawn "zeal")
  , TI "mcf"      "~/.mcf"                                   (spawnShell)
  , TI "pcl"      "~/Workspace/Libraries/pcl"                (spawnShell)
  , TI "opencv"   "~/Workspace/Libraries/opencv"             (spawnShell)
  , TI "v4r"      "~/Workspace/Projects/v4r"                 (spawnShell)
  , TI "ael"      "~/Workspace/Projects/aeolus"              (spawnShell)
  , TI "docker"   "~/Workspace/Projects/aeolus"              (spawnInShell "aeolus container-attach")
  , TI "mp3"      ""                                         (spawn "easytag" >> spawn "nautilus ~/Downloads/Torrents")
  , TI "profiler" ""                                         (spawn "profiler_gui")
  , TI "msg"      ""                                         (spawn "slack" >> spawn "telegram-desktop")
  , ti "gimp"     ""
  , ti "zoom"     ""
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

-- | Returns name and directory associated with current topic
currentTopicDirName tg = do
  topic <- gets (W.tag . W.workspace . W.current . windowset)
  return (topic, fromMaybe "" . M.lookup topic $ topicDirs tg)

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ appTerminal ++ " --working-directory " ++ dir

spawnInShell :: String -> X ()
spawnInShell cmd = spawn $ appTerminal ++ " -e '" ++ cmd ++ "'"

spawnShell :: X ()
spawnShell = currentTopicDirName myTopicConfig >>= appTmux

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
        h = 0.8             -- height
        w = 0.6             -- width
        t = (1.0 - h) / 2.0 -- centered top/bottom
        l = (1.0 - w) / 2.0 -- centered left/right

-- }}}
-- Key bindings ------------------------------------------------------------ {{{

-- Hint: use `xev -event keyboard | grep keysym` to figure out key names

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
  , k "v"            __                      shiftToWorkspace        __                      shiftAndGoToWorkspace
  , k "w"            gotoWorkspace'          shiftToWorkspace'       createWorkspace         shiftAndGoToWorkspace'
  , k "x"            __                      __                      __                      deleteWorkspace
  , k "y"            __                      __                      __                      __
  , k "z"            promptZealSearch        __                      __                      __
  , k "<Backspace>"  renameWorkspace         __                      __                      __
  , k "<Space>"      launchKupfer            nextKeyboardLayout      __                      __
  , k "<Tab>"        nextLayout              resetLayout             __                      __
  , k "`"            scratchTerminal         __                      __                      __
  , k "'"            gridSelect              __                      __                      __
  , k "/"            promptWebSearch         selectWebSearch         __                      __
  , k "0"            gotoPrevWorkspace       __                      __                      __
  , k "]"            windowOpacityUp         __                      __                      __
  , k "["            windowOpacityDown       __                      __                      __
  , k "<F5>"         __                      restartXMonad           __                      __
  , k "<F9>"         __                      __                      __                      lockScreen
  , k "<F10>"        __                      __                      __                      logout
  , k "<F11>"        toggleStruts            __                      __                      reboot
  , k "<F12>"        __                      __                      __                      shutdown
  , k "<Left>"       (snapMoveFloat L)       (snapGrowFloat L)       __                      __
  , k "<Right>"      (snapMoveFloat R)       (snapGrowFloat R)       __                      __
  , k "<Up>"         (snapMoveFloat U)       (snapGrowFloat U)       __                      __
  , k "<Down>"       (snapMoveFloat D)       (snapGrowFloat D)       __                      __
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
  , [bind "" "<XF86Favorites>"         sendKiss]
  , [bind "" "<Print>"                 printScreen]
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
    launchBrowser           = Unbound "Launch browser"                                     (spawn appBrowser)
    launchTerminal          = Unbound "Launch terminal"                                    (spawn appTerminal)
    launchKupfer            = Unbound "Launch kupfer"                                      (spawn "kupfer")
    -- Window navigation
    gotoNextWindow          = Unbound "Switch to next window"                              (windows W.focusDown)
    gotoMaster              = Unbound "Move focus to the master window"                    (windows W.focusMaster)
    goUp                    = Unbound "Switch to window above"                             (sendMessage $ Go U)
    goDown                  = Unbound "Switch to window below"                             (sendMessage $ Go D)
    goLeft                  = Unbound "Switch to window to the left"                       (sendMessage $ Go L)
    goRight                 = Unbound "Switch to window to the right"                      (sendMessage $ Go R)
    swapMaster              = Unbound "Swap with the master window"                        (windows W.swapMaster)
    swapUp                  = Unbound "Swap with window above"                             (sendMessage $ Swap U)
    swapDown                = Unbound "Swap with window below"                             (sendMessage $ Swap D)
    swapLeft                = Unbound "Swap with window to the left"                       (sendMessage $ Swap L)
    swapRight               = Unbound "Swap with window to the right"                      (sendMessage $ Swap R)
    -- Floating window manipulation
    moveUp                  = Unbound "Move floating window up"                            (withFocused (keysMoveWindow (0, -10)))
    moveDown                = Unbound "Move floating window down"                          (withFocused (keysMoveWindow (0, 10)))
    moveLeft                = Unbound "Move floating window left"                          (withFocused (keysMoveWindow (-10, 0)))
    moveRight               = Unbound "Move floating window right"                         (withFocused (keysMoveWindow (10, 0)))
    expandVertical          = Unbound "Expand floating window vertically"                  (withFocused (keysResizeWindow (0, 10) (0, 1%2)))
    expandHorizontal        = Unbound "Expand floating window horizontally"                (withFocused (keysResizeWindow (10, 0) (1%2, 0)))
    shrinkVertical          = Unbound "Shrink floating window vertically"                  (withFocused (keysResizeWindow (0, -10) (0, 1%2)))
    shrinkHorizontal        = Unbound "Shrink floating window horizontally"                (withFocused (keysResizeWindow (-10, 0) (1%2, 0)))
    snapMoveFloat dir       = Unbound "Snap floating window"                               (withFocused $ snapMove dir Nothing)
    snapGrowFloat dir       = Unbound "Snap and grow floating window"                      (withFocused $ snapGrow dir Nothing)
    -- Layout management
    shrinkMaster            = Unbound "Shrink master window"                               (sendMessage Shrink)
    expandMaster            = Unbound "Expand master window"                               (sendMessage Expand)
    nextLayout              = Unbound "Switch to next layout"                              (sendMessage NextLayout)
    resetLayout             = Unbound "Switch to default layout"                           (sendMessage FirstLayout)
    tileFloating            = Unbound "Push into tile"                                     (withFocused $ windows . W.sink)
    resizeFloatingWindow    = Unbound "Resize focused floating window"                     (withFocused $ FR.mouseResizeWindow)
    toggleMagnifier         = Unbound "Toggle magnifier"                                   (sendMessage Mag.Toggle)
    -- Workspace navigation
    gotoPrevWorkspace       = Unbound "Switch to previous workspace"                       (toggleWS' ["NSP"])
    gotoWorkspace'          = Unbound "Go to named workspace (auto-completion)"            (removeIfEmpty (DW.withWorkspace myXPConfigAutoComplete goto))
    shiftToWorkspace        = Unbound "Shift to named workspace"                           (removeIfEmpty (DW.withWorkspace myXPConfig sendX))
    shiftToWorkspace'       = Unbound "Shift to named workspace (auto-completion)"         (removeIfEmpty (DW.withWorkspace myXPConfigAutoComplete sendX))
    shiftAndGoToWorkspace   = Unbound "Shift and go to named workspace"                    (removeIfEmpty (DW.withWorkspace myXPConfig takeX))
    shiftAndGoToWorkspace'  = Unbound "Shift and go to named workspace (auto-completion)"  (removeIfEmpty (DW.withWorkspace myXPConfigAutoComplete takeX))
    nextWorkspace           = Unbound "Go to next workspace"                               (removeIfEmpty (DO.moveTo Next HiddenNonEmptyWS))
    prevWorkspace           = Unbound "Go to previous workspace"                           (removeIfEmpty (DO.moveTo Prev HiddenNonEmptyWS))
    createWorkspace         = Unbound "Create named workspace"                             (DW.selectWorkspace myXPConfig)
    renameWorkspace         = Unbound "Rename workspace"                                   (DW.renameWorkspace myXPConfig)
    deleteWorkspace         = Unbound "Remove workspace"                                   (DW.removeWorkspace)
    -- Misc
    toggleStruts            = Unbound "Toggle struts"                                      (sendMessage ToggleStruts)
    scratchTerminal         = Unbound "Open scratch terminal"                              (namedScratchpadAction myScratchPads "terminal")
    restartXMonad           = Unbound "Restart XMonad"                                     (spawn "killall polybar" <+> restart "xmonad" True)
    jumpToNextScreen        = Unbound "Jump to next physical screen"                       (onNextNeighbour def W.view)
    jumpToPrevScreen        = Unbound "Jump to previous physical screen"                   (onPrevNeighbour def W.view)
    lockScreen              = Unbound "Lock screen"                                        (spawn "session lock")
    logout                  = Unbound "Logout"                                             (spawn "session logout")
    reboot                  = Unbound "Reboot the system"                                  (spawn "session reboot")
    shutdown                = Unbound "Power off the system"                               (spawn "session shutdown")
    promptZealSearch        = Unbound "Prompt Zeal search"                                 (myPromptZealSearch)
    promptWebSearch         = Unbound "Prompt web search"                                  (submap . mySearchMap $ myPromptWebSearch)
    selectWebSearch         = Unbound "X selection web search"                             (submap . mySearchMap $ mySelectWebSearch)
    closeWindow             = Unbound "Close the focused window"                           (kill)
    gridSelect              = Unbound "Open GridSelect"                                    (goToSelected gridSelectConfig)
    sendKiss                = Unbound "Send kiss to Anja"                                  (spawn "kiss")
    printScreen             = Unbound "Print screen using Flameshot"                       (spawn "flameshot gui")
    -- Keyboard control
    nextKeyboardLayout      = Unbound "Next keyboard layout"                               (spawn "keyboard -n")
    -- Audio control
    audioMute               = Unbound "Mute audio"                                         (spawn "amixer -D pulse set Master toggle")
    audioLowerVolume        = Unbound "Lower audio volume"                                 (spawn "amixer -D pulse set Master 5%-")
    audioRaiseVolume        = Unbound "Raise audio volume"                                 (spawn "amixer -D pulse set Master 5%+")
    audioPlay               = Unbound "Play/pause audio playback"                          (spawn "mpc toggle")
    audioStop               = Unbound "Stop audio playback"                                (spawn "mpc stop")
    audioRate rating        = Unbound "Rate current song"                                  (spawn ("mpdrate " ++ show rating))
    -- Brightness control
    brightnessDown          = Unbound "Brightness down"                                    (spawn "xbacklight -dec 1")
    brightnessUp            = Unbound "Brightness up"                                      (spawn "xbacklight -inc 1")
    -- Window opacity control
    windowOpacityUp         = Unbound "Window opacity up"                                  (spawn "transset -a --inc 0.1")
    windowOpacityDown       = Unbound "Window opacity down"                                (spawn "transset -a --dec 0.1")

-- Two varieties of Action: B(ound) is aware of the key that was used to
-- invoke it, U(nbound) is not aware of the key.
data Action = Unbound String (          X ()) |
              Bound   String (String -> X ())

gotoX = windows . W.view
sendX = windows . W.shift
takeX = sendX ->> gotoX

removeIfEmpty = DW.removeEmptyWorkspaceAfterExcept myTopicNames

-- Helpers for performing multiple actions on the same entity
infixl 1 ->>
(a ->> b) c = do a c
                 b c

-- }}}
-- Grid Select ------------------------------------------------------------ {{{

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap = M.fromList [ ((0, xK_Escape), cancel)
                           , ((0, xK_Return), select)
                           , ((0, xK_c), move (0, -1) >> myNavigation)
                           , ((0, xK_t), move (0, 1) >> myNavigation)
                           , ((0, xK_n), move (1, 0) >> myNavigation)
                           , ((0, xK_h), move (-1, 0) >> myNavigation)
                           ]

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

-- Prompt Zeal search: get input from the user via a prompt, then run Zeal
myPromptZealSearch = inputPrompt myXPConfig "Search Zeal" ?+ \s ->
      (spawn ("zeal " ++ s))

-- Prompt web search: get input from the user via a prompt, then run the search in
-- the browser and automatically switch to the web workspace
myPromptWebSearch (SearchEngine name site)
  = inputPrompt myXPConfig ("Search " ++ name) ?+ \s ->
      (search appBrowser site s >> viewWeb)

-- Select search: do a web search based on the X selection
mySelectWebSearch eng = selectSearch eng >> viewWeb

-- Switch to the "web" workspace
viewWeb = windows (W.view "web")

-- }}}
-- Main -------------------------------------------------------------------- {{{

-- desktop :: DESKTOP_SESSION -> desktop configuration
desktop "gnome"  = gnomeConfig
desktop "xmonad" = gnomeConfig
desktop "plasma" = kde4Config
desktop _        = desktopConfig

main :: IO ()
main = do
  session          <- getEnv "DESKTOP_SESSION"
  dbus             <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log") [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  let myDesktopConfig = maybe desktopConfig desktop session
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ ED.ewmh myDesktopConfig
    { modMask            = mod4Mask          -- changes the mode key to "super"
    , focusedBorderColor = colorBorderActive -- color of focused border
    , normalBorderColor  = colorBg           -- color of inactive border
    , borderWidth        = 1                 -- width of border around windows
    , terminal           = appTerminal       -- default terminal program
    , workspaces         = myTopicNames
    , manageHook         = manageHook myDesktopConfig <+> myManageHook
    , logHook            = do
        updatePointer (0.5, 0.5) (0, 0)
        logHook myDesktopConfig
        dynamicLogWithPP (logHookPolybar dbus)
    , layoutHook         = myLayoutHook
    , handleEventHook    = myHandleEventHook
    , startupHook        = setWMName "LG3D" <+> spawn pathPolybar
    }
    `additionalKeysP`
    myKeyBindingsTable
    `additionalMouseBindings`
    [ ((mod4Mask, button2), (\w -> focus w >> FR.mouseResizeWindow w)) ]

-- }}}
-- Status bar -------------------------------------------------------------- {{{

xdoMod :: String -> String
xdoMod key = "/usr/bin/xdotool key super+" ++ key

xdoModCtrl :: String -> String
xdoModCtrl key = "/usr/bin/xdotool key super+ctrl+" ++ key

addSpaces :: String -> String
addSpaces = init . go
  where go [] = []
        go xs = let (as, bs) = splitAt 1 xs in as ++ (' ' : go bs)

xdoGotoWorkspace :: String -> String
xdoGotoWorkspace ws = xdoModCtrl "w" ++ " " ++ addSpaces ws ++ " KP_Enter"

clickable :: String -> String
clickable ws = polybarAction (xdoGotoWorkspace ws) 1 ws

myWorkspaceSorter = do
  srt <- fmap (namedScratchpadFilterOutWorkspace.) (getSortByXineramaPhysicalRule def)
  return srt

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) { D.signalBody = [D.toVariant $ UTF8.decodeString str] }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

logHookPolybar dbus = def
  { ppOutput           = dbusOutput dbus
  , ppSort             = myWorkspaceSorter
  , ppOrder            = \(ws:l:t:x) -> [ws, l, t] ++ x
  , ppSep              = "   "
  , ppWsSep            = " "
  , ppCurrent          = polybarColor "#ffffff" solarizedCyan . wrap " " " "
  , ppUrgent           = polybarColor colorFgPrimary solarizedViolet . wrap " " " " . clickable
  , ppVisible          = polybarColor colorFgPrimary "" . wrap " " " " . clickable
  , ppHidden           = clickable
  , ppHiddenNoWindows  = const ""
  , ppTitle            = (" " ++) . polybarColor colorFgPrimary "" . shorten 50
  , ppLayout           = polybarAction (xdoMod "Tab") 1 .
    (\x -> case x of
    "MouseResizableTile"        -> "[T]"
    "Mirror MouseResizableTile" -> "[M]"
    "Grid"                      -> "[G]"
    "Tabbed Simplest"           -> "[F]"
    "Tabbed Bottom Simplest"    -> "[F]"
    _ -> x
    )
  }

-- }}}
-- HandleEvent hook -------------------------------------------------------- {{{

myHandleEventHook = ED.fullscreenEventHook

-- }}}
-- Manage hook ------------------------------------------------------------- {{{

myManageHook :: ManageHook
myManageHook = manageWindows <+> namedScratchpadManageHook myScratchPads <+> fullscreenManageHook

manageWindows = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doFloat   | c <- myCFloats]
    , [className =? u --> doUnfloat | u <- myCUnFloats] -- does not work for Gimp actually
    , [title     =? t --> doFloat   | t <- myTFloats]
    , [resource  =? r --> doFloat   | r <- myRFloats]
    , [resource  =? i --> doIgnore  | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "gimp" | x <- myGimpShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "zoom" | x <- myZoomShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "mendeley" | x <- myMendeleyShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "rviz" | x <- myRVizShifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    doUnfloat = ask >>= doF . W.sink
    myCFloats = ["Exe", "plasmashell", "Plasma-desktop"]
    myCUnFloats = ["Gimp", "Nautilus"]
    myTFloats = ["Downloads", "Save As...", "Export Image"]
    myRFloats = []
    myIgnores = []
    myGimpShifts = ["Gimp"]
    myZoomShifts = ["zoom", "Zoom"]
    myMendeleyShifts = ["Mendeley Desktop"]
    myRVizShifts = ["RViz", "rviz", "Gazebo"]

-- Hint: use `xprop` to figure out window class name

-- }}}
