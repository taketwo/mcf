--- Large sections of this file come from
--- github.com/davidbrewer/xmonad-ubuntu-conf

-- Language
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses,  ImplicitParams, PatternGuards #-}

import XMonad hiding ( (|||) )
import XMonad.Core
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
myBrowser  = "chromium-browser"
myShell    = "bash"
myFont     = "Liberation Mono"

-- }}}
-- Appearance -------------------------------------------------------------- {{{

xpFont               = "xft:" ++ myFont ++ ":pixelsize=11"
dzenFont             = "-*-" ++ myFont ++ "-*-r-normal-*-11-*-*-*-*-*-*-*"
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
topLeftBarWidth      = 600
topBarTitleLength    = 80

myTabConfig = defaultTheme
  { activeColor         = dzenBg
  , activeTextColor     = dzenFgLight
  , activeBorderColor   = solarizedOrange
  , inactiveColor       = dzenBg
  , inactiveTextColor   = dzenFgDark
  , inactiveBorderColor = dzenBg
  , fontName            = xpFont
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

myCode = windowNavigation $ limitWindows 3 $ Mag.magnifiercz' 1.4 $ mouseResizableTile { draggerType = BordersDragger }

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayoutHook =
-- start all workspaces in my home directory, with the ability
-- to switch to a new working dir
  workspaceDir "~" $
  onWorkspace "figures" (windowNavigation $ Mag.magnifierOff $ GridRatio (4/3)) $
  {-$ onWorkspace "9:Pix" gimpLayout-}
  {-$-} defaultLayouts

-- }}}
-- Topics ------------------------------------------------------------------ {{{

data TopicItem = TI { topicName   :: Topic
                    , topicDir    :: Dir
                    , topicAction :: X ()
                    }

myTopics :: [TopicItem]
myTopics =
  [ TI "web"   ""      (spawn myBrowser)
  , TI "im"    ""      (spawn "skype")
  , TI "music" ""      (spawn "rhythmbox")
  {-, TI "mail" "" (runInTerm "" "ssh en")-}
  {-, ti "read" "papers"-}
  {-, ti "write" "writing/blog/stream-comonad"-}
  {-, TI "org" "notes"-}
    {-(spawn "emacs --name org ~/notes/journal.org")-}
  {-, TI "draw" "" (spawn "inkscape")-}
  , TI "xmonad" ".xmonad" (edit "/home/sergey/.mcf/.xmonad/xmonad.hs")
  , TI "mcf"    ".mcf"    (spawnShell)
  , TI "ipy"    ""    (spawnInShell "ipython")
  {-, ti "xm-hack" "src/xmonad/XMonadContrib"-}
  {-, TI "em-conf" "" (edit "~/.emacs")-}
  {-, TI "net" "" (spawn "wicd-client -n" >>-}
                 {-shell)-}
  , ti "papers" ""
  , ti "figures" ""
  , ti "gimp" ""
  {-, ti "500" "teaching/500/sf"-}
  {-, ti "ref" "documents/reference"-}
  {-, ti "play" ""-}
  {-, TI "tex-conf" "texmf/tex" (edit "~/texmf/tex/brent.sty")-}
  {-, ti "mlt" "writing/mlt"-}
  {-, ti "MR" "writing/Monad.Reader/issues/Issue19"-}
  {-, ti "mc" "teaching/mathcounts"-}
  {-, TI "video" "video" (spawn "cinelerra")-}
  {-, TI "aop" "learning/aop" (spawnShell host-}
                             {->> spawn "emacs ~/learning/aop/aop.lhs")-}
  {-, ti "tc" "writing/typeclassopedia"-}
  {-, ti "noah" "documents/noah/schedule"-}
  ]
  where
    -- Make a default topic item that just spawns a shell.
    ti t d = TI t d spawnShell

edit :: String -> X ()
edit f = spawn (myTerminal ++ " -e 'vim " ++ f ++ "'")

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
spawnShellIn dir = spawn $ myTerminal ++ " --working-directory " ++ dir

spawnInShell :: String -> X ()
spawnInShell cmd = spawn $ myTerminal ++ " -e '" ++ cmd ++ "'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfigAutoComplete goto

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
-- Key bindings ------------------------------------------------------------ {{{

myKeyBindingsTable = concat $ table

--        key                  M-              M-S-           M-C-           M-S-C-
table =
  [ k "<Return>"     launchTerminal         __              __                __
  {-, k "a"            gotoScreen1      sendScreen1   takeScreen1      swapScreen1-}
  , k "b"            gotoWorkspace    shiftToWorkspace createWorkspace    shiftAndGoToWorkspace
  {-, k "b"            __               __            openBrowser        __-}
  , k "c"            goUp             swapUp        launchCalendar       shrinkMaster
  {-, k "d"            launchWithDmenu      __              __                __-}
  {-, k "e"            wicdNetwork          __              __                __-}
  , k "f"            __               tileFloating        __                __
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
  , k "w"            nextWorkspace    prevWorkspace renameWorkspace' deleteWorkspace
  , k "x"                __               __              __                __
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
    launchTerminal          = Unbound "Launch terminal"                 (spawn myTerminal)
    launchKupfer            = Unbound "Launch kupfer"                   (spawn "kupfer")
    launchCalendar          = Unbound "Launch calendar"                 (spawn "chromium-browser --app-id = ejjicmeblgpmajnghnpcppodonldlgfn")
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

    {-gotoRecentWS     = Unbound "Switch to the most recently visited invisible workspace" (windows gotoRecent)-}
    {-sendRecentWS     = Unbound   "Send to the most recently visited invisible workspace" (windows sendRecent)-}
    {-takeRecentWS     = Unbound   "Take to the most recently visited invisible workspace" (windows takeRecent)-}

-- Two varieties of Action: B(ound) is aware of the key that was used to
-- invoke it, U(nbound) is not aware of the key.
data Action = Unbound String (          X ()) |
              Bound   String (String -> X ())


myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font                = xpFont
  , bgColor             = dzenBg
  , fgColor             = dzenFgLight
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
      (search myBrowser site s >> viewWeb)

-- Select search: do a search based on the X selection
mySelectSearch eng = selectSearch eng >> viewWeb

-- Switch to the "web" workspace
viewWeb = windows (W.view "web")

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
  dzensTopLeft <- mapM (spawnPipe . dzenCommand) [1 .. screenCount]
  xmonad $ gnomeConfig
    { modMask            = mod4Mask         -- changes the mode key to "super"
    , focusedBorderColor = solarizedOrange  -- color of focused border
    , normalBorderColor  = dzenBg           -- color of inactive border
    , borderWidth        = 1                -- width of border around windows
    , terminal           = "gnome-terminal" -- default terminal program
    , workspaces         = myTopicNames
    , manageHook = manageHook gnomeConfig <+> myManageHook
    , logHook = (mapM_ dynamicLogWithPP $ zipWith (logHookTopLeft pathIcons) dzensTopLeft [1 .. screenCount])
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
myIconPath = "/home/sergey/.xmonad/icons/"
wrapTextBox :: String -> String -> String -> String -> String
wrapTextBox fg bg1 bg2 t = "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath  ++ "boxleft.xbm)^ib(1)^r(" ++ show xRes ++ "x" ++ show topBarBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg(" ++ fg ++ ")" ++ t ++ "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath ++ "boxright.xbm)^fg(" ++ bg2 ++ ")^r(" ++ show xRes ++ "x" ++ show topBarBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg()^ib(0)"

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
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
  , ppOrder           = \(ws:l:t:x) -> [ws, l, t] ++ x
  , ppSep             = " "
  , ppWsSep           = ""
  , ppCurrent         =                      wrapTextBox dzenFgDark  dzenFgLight dzenBg
  , ppUrgent          =                      wrapTextBox dzenUrgent  dzenBg      dzenBg
  , ppVisible         =                      wrapTextBox dzenFgLight dzenFgDark  dzenBg
  , ppHiddenNoWindows = const ""
  , ppHidden          =                      wrapTextBox dzenFgLight dzenBg      dzenBg
  , ppTitle           = (" " ++)           . dzenColor   dzenFgLight dzenBg             . dzenEscape . shorten topBarTitleLength
  , ppLayout          = wrapClickLayout    . dzenColor   dzenFgDark  dzenBg             .
    (\x -> case x of
    "MouseResizableTile"                    -> "^i(" ++ icons ++ "/tall.xbm)"
    "Mirror MouseResizableTile"             -> "^i(" ++ icons ++ "/mtall.xbm)"
    "Grid"                                  -> "^i(" ++ icons ++ "/grid.xbm)"
    "Tabbed Bottom Simplest"                -> "^i(" ++ icons ++ "/full.xbm)"
    "Magnifier NoMaster MouseResizableTile" -> "^i(" ++ icons ++ "/code.xbm)"
    _ -> x
    )
  }

-- }}}
-- }}}
-- HandleEvent hook -------------------------------------------------------- {{{

myHandleEventHook = ED.fullscreenEventHook <+> docksEventHook

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
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "gimp" | x <- myGimpShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "papers" | x <- myPapersShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "im" | x <- myIMShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "music" | x <- myMusicShifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = []
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = []
    myGimpShifts = ["Gimp"]
    myPapersShifts = ["Mendeley Desktop"]
    myMusicShifts = ["Rhythmbox", "Workrave"]
    myIMShifts = ["Skype"
                 , "crx_kbpgddbgniojgndnhlkjbkpknjhppkbk" -- Google+ Hangouts application
                 , "crx_nckgahadagoaajjgafhacjanaoiihapd" -- Google+ Hangouts extension
                 ]
-- }}}
