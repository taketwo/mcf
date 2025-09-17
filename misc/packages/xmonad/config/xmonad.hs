{-# LANGUAGE ImplicitParams, FlexibleContexts #-}

import XMonad
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
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Column
import XMonad.Layout.ComboP
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier as Mag
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
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
import qualified XMonad.StackSet as W

import Control.Monad (liftM2)
import Data.Ratio ((%))
import System.Directory
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (maybe, fromMaybe)
import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import qualified Data.Map as M

import MCF.Apps
import MCF.OneDark
import MCF.Paths
import MCF.Polybar

-- Appearance -------------------------------------------------------------- {{{

fontSpec             = "xft:VictorMono Nerd Font Mono:bold:pixelsize=13"
colorBg              = oneDarkBg0
colorBgAccent        = oneDarkPurple
colorFgAccent        = "#ffffff"     -- Bright foreground
colorFgPrimary       = oneDarkFg     -- Normal foreground
colorFgSecondary     = oneDarkGrey   -- Dimmed foreeground
colorBorder          = oneDarkBlack
colorBorderActive    = oneDarkBg3
barHeight            = 22

myTabConfig :: Theme
myTabConfig = def
  { activeColor         = colorBorderActive
  , activeTextColor     = colorFgAccent
  , activeBorderColor   = colorBorderActive
  , urgentColor         = colorBgAccent
  , urgentTextColor     = colorFgAccent
  , urgentBorderColor   = colorBg
  , inactiveColor       = colorBg
  , inactiveTextColor   = colorFgSecondary
  , inactiveBorderColor = colorBg
  , fontName            = fontSpec
  }

myXPConfig :: XPConfig
myXPConfig = def
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

layoutMsg = fullscreenFull $ reflectVert $ named "msg" (combineTwoP (Column 3.0) (tabbed shrinkText myTabConfig) (tabbedBottom shrinkText myTabConfig) (ClassName "Slack"))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayoutHook =
-- start all workspaces in my home directory, with the ability
-- to switch to a new working dir
  onWorkspace "msg" (layoutMsg) $
  {-$-} defaultLayouts

-- }}}
-- Topics ------------------------------------------------------------------ {{{

myTopics :: [TopicItem]
myTopics =
  [ TI "web"      ""                                         (spawn appBrowser)
  , TI "music"    ""                                         (spawnInTerminal "ncmpcpp")
  , TI "zeal"     ""                                         (spawn "zeal")
  , TI "mcf"      "~/.mcf"                                   (spawnTmux)
  , TI "ael"      ""                                         (spawnInTerminal "ael tmux load ael")
  , TI "alpaca"   ""                                         (spawnInTerminal "mosh alpaca")
  , TI "remmina"  ""                                         (spawn "remmina")
  , TI "robots"   ""                                         (spawnInTerminal "ael tmux load robots")
  , TI "docker"   ""                                         (spawnInTerminal "ael container attach")
  , TI "msg"      ""                                         (spawn "slack" >> spawn "telegram-desktop")
  , ti "gimp"     ""
  , ti "zoom"     ""
  ]
  where
    -- Make a default topic item that just spawns a shell.
    ti t d = TI t d spawnTmux

myTopicNames :: [Topic]
myTopicNames = map tiName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = def
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n, d)) myTopics
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n, a)) myTopics
  , defaultTopicAction = const (return ())
  , defaultTopic = "web"
  }

-- | Returns name and directory associated with current topic
currentTopicDirName tg = do
  topic <- gets (W.tag . W.workspace . W.current . windowset)
  return (topic, fromMaybe "" . M.lookup topic $ topicDirs tg)

-- Spawn tmux for a given topic
spawnTmux :: X ()
spawnTmux = currentTopicDirName myTopicConfig >>= spawnTmuxSession

-- Spawn tmux session in a given working directory.
-- If corresponding session file exists in $MCF/tmux/sessions/, load it.
spawnTmuxSession :: (String, String) -> X ()
spawnTmuxSession (session, dir) = do
  let sessionFile = pathMCF ++ "/tmux/sessions/" ++ session ++ ".yaml"
  exists <- io $ doesFileExist sessionFile
  let command = if exists
                then "tmuxp load " ++ sessionFile
                else "tmux new -s " ++ session
  spawn $ appTerminal ++ " --working-directory " ++ dir ++ " -e " ++ command


goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfigAutoComplete goto

-- }}}
-- Scratchpads ------------------------------------------------------------- {{{

myScratchPads = [ NS "terminal" spawnTerminal findTerminal manageTerminal ]
  where
    spawnTerminal  = appTerminal ++ " --class ScratchTerminal -e tmux new-session -A -s scratch"
    findTerminal   = (resource =? "ScratchTerminal")
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
  [ k "<Return>"     launchTmux              __                      __                      __
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
  , k "<Space>"      launchRofi              nextKeyboardLayout      __                      __
  , k "<Tab>"        nextLayout              resetLayout             __                      __
  , k "`"            scratchTerminal         __                      __                      __
  , k "'"            goToGridSelection       bringGridSelection      __                      __
  , k "/"            promptWebSearch         selectWebSearch         __                      __
  , k "0"            gotoPrevWorkspace       __                      __                      __
  , k "]"            windowOpacityUp         __                      __                      __
  , k "["            windowOpacityDown       __                      __                      __
  , k "<F4>"         (screenLayout "default")  (screenLayout "laptop")   __                          __
  , k "<F5>"         restartPolybar          restartXMonad           __                      __
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
  , [bind "" "<Print>"                 flameshotGUI]
  , [bind "M1-" "<Print>"              flameshotLauncher]
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
    launchTmux              = Unbound "Launch tmux"                                        (spawn appTmux)
    launchRofi              = Unbound "Launch rofi"                                        (spawn "rofi -show drun")
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
    moveUp                  = Unbound "Move floating window up"                            (withFocused (keysMoveWindow (0, fromIntegral(-10))))
    moveDown                = Unbound "Move floating window down"                          (withFocused (keysMoveWindow (0, 10)))
    moveLeft                = Unbound "Move floating window left"                          (withFocused (keysMoveWindow (fromIntegral(-10), 0)))
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
    createWorkspace         = Unbound "Create named workspace"                             (DW.selectWorkspace myXPConfig)
    renameWorkspace         = Unbound "Rename workspace"                                   (DW.renameWorkspace myXPConfig)
    deleteWorkspace         = Unbound "Remove workspace"                                   (DW.removeWorkspace)
    -- Misc
    toggleStruts            = Unbound "Toggle struts"                                      (sendMessage ToggleStruts)
    scratchTerminal         = Unbound "Open scratch terminal"                              (namedScratchpadAction myScratchPads "terminal")
    restartPolybar          = Unbound "Restart polybar"                                    (spawn "killall polybar" <+> spawn pathPolybar)
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
    goToGridSelection       = Unbound "Go to grid selection"                               (goToSelected $ gridSelectConfig)
    bringGridSelection      = Unbound "Bring grid selection"                               (bringSelected $ gridSelectConfig)
    flameshotGUI            = Unbound "Run Flameshot GUI"                                  (spawn "flameshot gui")
    flameshotLauncher       = Unbound "Run Flameshot launcher"                             (spawn "flameshot launcher")
    screenLayout layout     = Unbound "Set screen layout"                                  (spawn ("screen-layout " ++ layout))
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
gridSelectConfig = def { gs_navigate = myNavigation
                       , gs_font = fontSpec }

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

main :: IO ()
main = do
  dbus             <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log") [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  maybeSession <- lookupEnv "DESKTOP_SESSION"
  let myDesktopConfig = case maybeSession of
        Just "gnome"  -> gnomeConfig
        Just "xmonad" -> gnomeConfig
        Just "plasma" -> kde4Config
        _             -> desktopConfig
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ ewmhFullscreen $ ewmh myDesktopConfig
    { modMask            = mod4Mask          -- changes the mode key to "super"
    , focusedBorderColor = colorBorderActive -- color of focused border
    , normalBorderColor  = colorBorder       -- color of inactive border
    , borderWidth        = 1                 -- width of border around windows
    , terminal           = appTerminal       -- default terminal program
    , workspaces         = myTopicNames
    , manageHook         = manageHook myDesktopConfig <+> myManageHook
    , logHook            = do
        updatePointer (0.5, 0.5) (0, 0)
        logHook myDesktopConfig
        dynamicLogWithPP (logHookPolybar dbus)
    , layoutHook         = myLayoutHook
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
  srt <- fmap (filterOutWs [scratchpadWorkspaceTag].) (getSortByXineramaPhysicalRule def)
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

logHookPolybar :: D.Client -> PP
logHookPolybar dbus = def
  { ppOutput           = dbusOutput dbus
  , ppSort             = myWorkspaceSorter
  , ppOrder            = \(ws:l:t:x) -> [ws, l, t] ++ x
  , ppSep              = "   "
  , ppWsSep            = " "
  , ppCurrent          = polybarColor oneDarkBg3 oneDarkGreen . wrap " " " "
  , ppUrgent           = polybarColor colorFgAccent colorBgAccent . wrap " " " " . clickable
  , ppVisible          = polybarColor colorFgPrimary "" . wrap " " " " . clickable
  , ppHidden           = clickable
  , ppHiddenNoWindows  = const ""
  , ppTitle            = (" " ++) . polybarColor colorFgPrimary "" . shorten 70
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
-- Manage hook ------------------------------------------------------------- {{{

myManageHook :: ManageHook
myManageHook = manageWindows <+> namedScratchpadManageHook myScratchPads <+> fullscreenManageHook

manageWindows = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [isFloating --> doCenterFloat]
    , [isOSD --> doCenterFloat]
    , [isTooltip --> doIgnore]
    , [className =? c --> doFloat   | c <- myCFloats]
    , [className =? u --> doUnfloat | u <- myCUnFloats] -- does not work for Gimp actually
    , [title     =? t --> doFloat   | t <- myTFloats]
    , [resource  =? r --> doFloat   | r <- myRFloats]
    , [resource  =? i --> doIgnore  | i <- myIgnores]
    , [className =? c --> doF W.focusDown <+> doIgnore  | c <- myCUnFocus]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "gimp" | x <- myGimpShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "zoom" | x <- myZoomShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "mendeley" | x <- myMendeleyShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "sim" | x <- mySimShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "uvc" | x <- myUvcShifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    doUnfloat = ask >>= doF . W.sink
    myCFloats = ["Exe", "Plasma-desktop"]
    myCUnFocus = []
    myCUnFloats = ["Gimp", "Nautilus"]
    myTFloats = ["Downloads", "Save As...", "Export Image", "zoom_linux_float_video_window", "as_toolbar", "cpt_frame_window"]
    myRFloats = []
    myIgnores = ["plasmashell"]
    myGimpShifts = ["Gimp"]
    myZoomShifts = ["zoom", "Zoom"]
    myMendeleyShifts = ["Mendeley Desktop"]
    mySimShifts = ["RViz", "rviz", "Gazebo", "aeolus_rviz_launcher"]
    myUvcShifts = ["UVC Coverage GUI"]
    isFloating = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE"
    isOSD      = isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_ON_SCREEN_DISPLAY"
    isTooltip  = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_TOOLTIP"

-- Hint: use `xprop` to figure out window class name

-- }}}
