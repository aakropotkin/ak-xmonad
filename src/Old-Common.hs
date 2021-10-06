-- Language {{{4
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- }}}4

module XMonadCommon where

--------------------------------------------------------------------------------
-- Imports {{{4
--------------------------------------------------------------------------------
import Control.Monad
import Data.Monoid
import Data.Ratio
import XMonad
import XMonad.Core as XMonad hiding
  ( workspaces, manageHook, keys, logHook, startupHook, borderWidth
  , mouseBindings, layoutHook, modMask, terminal, normalBorderColor
  , focusedBorderColor, focusFollowsMouse, handleEventHook, clickJustFocuses
  , rootMask, clientMask)
import qualified XMonad.Core as XMonad
import XMonad.Actions.Commands (defaultCommands)
import XMonad.Actions.WindowGo
import XMonad.Core (X, withDisplay, io)
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ServerMode
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Layout.Spacing
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste
import Data.List

-- My Modules:
import XMonadTheme
import KeyList
-- }}}4

--------------------------------------------------------------------------------
-- Directories and Program Defaults {{{2
--------------------------------------------------------------------------------

-- Directories {{{3
data Paths = Paths {
    xResources
  , xMonad
  , scripts
  , note
  , screenshots
  , bin :: String
  }
-- }}}3

-- Workspaces
myWorkspaces = map show [1..7] ++ [ "Chat", "Music" ]

-- Programs
myTerm      = "urxvtc"
myTermFull  = myTerm ++ " -tr -sh 8"
myBrowser   = "chromium"
-- END Directories and Programs }}}2

--------------------------------------------------------------------------------
-- Commands {{{3
--------------------------------------------------------------------------------

allCommands = do
  cmds <- defaultCommands
  return $ cmds ++ myCommands

myCommands :: [(String, X ())]
myCommands =
  -- Simulate Keypresses
  --[ ("sendKey" ++ show s, sendKey 0 k) | (k, s) <- keyList ] ++
  [ ("Dictionary", namedScratchpadAction myPads "dictionary")
  , ("Translate", namedScratchpadAction myPads "translate")
  , ("NixPkgs Search", namedScratchpadAction myPads "nixpkgs-search")
  , ("NixOpts Search", namedScratchpadAction myPads "nixopts-search")
  , ("Home-Opts Search", namedScratchpadAction myPads "home-manager-search")
  ]

lookupInv :: (Eq a, Eq b) => b -> [(a, b)] -> Maybe (a, b)
lookupInv x = find ((== x) . snd)

-- Adds all keymaps as commands
doSendKey string = case lookupInv key keyList of
  (Just (i,_)) -> sendKey mask i
  otherwise    -> return ()
  where mask = case unwords . init $ words string of
          "shiftMask" -> shiftMask
          "mod1Mask"  -> mod1Mask
          "mod4Mask"  -> mod4Mask
          "(shiftMask .|. mod1Mask)" -> (shiftMask .|. mod1Mask)
          "(mod1Mask .|. shiftMask)" -> (shiftMask .|. mod1Mask)
          "(shiftMask .|. mod4Mask)" -> (shiftMask .|. mod4Mask)
          "(mod4Mask .|. shiftMask)" -> (shiftMask .|. mod4Mask)
          n -> read n :: KeyMask
        key    = last $ words string

-- }}}3

--------------------------------------------------------------------------------
-- Hooks {{{2
--------------------------------------------------------------------------------

-- Window Management {{{3
myManageHook = composeAll
    [ manageDocks
    , myManageScratch
    , namedScratchpadManageHook myPads
    , role      =? "Spotify"      --> viewShift "Music"
    , className =? "Xmessage"     --> doCenterFloat
    , className =? "Pavucontrol"  --> doCenterFloat
    , title     =? "Calendar"     --> doFloat
    , title     =? "XScreenfetch" --> doCenterFloat
    , title     =? "OpenGL"       --> doCenterFloat
    , title     =? "Float"        --> doFloat
    --, title     =? "glirc2"       --> viewShift "Chat"
    , title     =? "finch"        --> viewShift "Chat"
    , title     =? "weechat"      --> viewShift "Chat"
    , isFullscreen                --> doFullFloat
    , isDialog                    --> doCenterFloat
    , manageHook def
    ] :: ManageHook
-- }}}3

-- Dynamic Window Roles Management {{{3
myDynRoleHook paths = composeAll
    [ role    =? "Spotify" --> viewShift "Music"
    , appName =? "urxvt"   --> unfloat <+> (doShift =<< currentWs)
    ] :: ManageHook

myDynTitleHook paths = composeAll
  [ role =? "Spotify" --> (idHook =<< liftX
      (spawn $ "notify-send \"$(" ++ scripts paths ++ "/spotinfo)\""))
  ] :: ManageHook
-- }}}3

-- Scratchpads {{{3
myManageScratch :: ManageHook -- {{{4
myManageScratch = scratchpadManageHook dropDownFloating
myPads = [
    scratchCommand "nixopts-search"      "nix-search -o" dropDownFloating
  , scratchCommand "nixpkgs-search"      "nix-search"    dropDownFloating
  , scratchCommand "home-manager-search" "nix-search -h" dropDownFloating
  , scratchCommand "wiki"                "wiki"          dropDownFloating
  , scratchCommand "dictionary"          "dicti"         dropDownFloating
  , scratchCommand "translate"           "trans -I"      rightFloating
  , NS "IRC" (namedTermWith' "IRC" "glirc2") (title =? "glirc2") defaultFloating
  ] --}}}4

scratchCommand :: String -> String -> W.RationalRect -> NamedScratchpad -- {{{5
scratchCommand name cmd dims = NS name (namedTermWith' name cmd)
  (appName =? name) (customFloating dims)
-- }}}5

-- Scratch Window Sizes {{{5
screenHeight, screenWidth :: Integer
screenHeight = 1080 -- sometimes 1050
screenWidth  = 1920 -- sometimes 1680

-- The size of a single gap or Xmobar as a % of total screen width
horizGapPerc, vertGapPerc, vertBarPerc :: Rational
horizGapPerc = (gaps + toInteger myBorderWidth) % screenWidth
vertGapPerc  = (gaps + toInteger myBorderWidth) % screenHeight
vertBarPerc  = toInteger (barsBorder + barsHeight) % screenHeight
-- }}}5

dropDownFloating = W.RationalRect fLeftX fTopY fWidth fHeight -- {{{4
  where fHeight = 1 / 3
        fWidth  = 1.0 - 2*horizGapPerc
        fTopY   = vertBarPerc + vertGapPerc
        fLeftX  = horizGapPerc
        --sL = 1.0 - sW - toInteger gaps % screenWidth --Left
        fHeight, fWidth, fTopY, fLeftX :: Rational

rightFloating   = W.RationalRect fLeftX fTopY fWidth fHeight
  where fHeight = 1.0 - 2*vertGapPerc - vertBarPerc
        fWidth  = 1 / 3
        fTopY   = vertBarPerc + vertGapPerc
        fLeftX  = (2 / 3) - horizGapPerc
        fHeight, fWidth, fTopY, fLeftX :: Rational
-- }}}4
-- }}}3

-- Event Handling {{{3
myHandleEventHook paths = composeAll
    [ fullscreenEventHook
    , dynamicPropertyChange "WM_WINDOW_ROLE" (myDynRoleHook paths)
    , dynamicPropertyChange "WM_CLASS" (myDynRoleHook paths)
    , dynamicPropertyChange "WM_NAME" (myDynTitleHook paths)
    , serverModeEventHook
    , serverModeEventHookF "sendKey" doSendKey
    , handleEventHook def
    ] :: Event -> X Data.Monoid.All
-- }}}3

-- Notifications {{{3
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    Just idx <- W.findTag w <$> gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]
-- }}}3
-- END Hooks }}}2

--------------------------------------------------------------------------------
-- Layout {{{2
--------------------------------------------------------------------------------

myLayoutHook = tiled ||| Mirror tiled ||| Full
    where -- spacingRaw smartBorder Border screenBorder Border windowBorder
        tiled = spacingRaw False border True border True $
                  Tall nmaster delta ratio
        border = Border gaps gaps gaps gaps
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100
-- }}}2

--------------------------------------------------------------------------------
-- Theme {{{2
--------------------------------------------------------------------------------

gaps          = 5  :: Integer
barsHeight    = 42 :: Dimension
barsBorder    = 3  :: Dimension
myBorderWidth = 2  :: Dimension

-- Prompt Config
mkXPConfig :: Theme -> XPConfig
mkXPConfig theme = def
    { font                 = "xft:" ++ tFont theme ++
                                ":size=" ++ show (tFontSize theme)
    , bgColor              = tBackground theme
    , fgColor              = tForeground theme
    , fgHLight             = tForegroundHi theme
    , bgHLight             = tBackgroundHi theme
    , borderColor          = tBorder theme
    --, promptKeymap         = vimLikeXPKeymap -- Need xmonad-confib's master
    , promptBorderWidth    = barsBorder
    , height               = barsHeight
    , showCompletionOnTab  = True
    }
-- }}}2

--------------------------------------------------------------------------------
-- Helpers {{{2
--------------------------------------------------------------------------------

role = stringProperty "WM_WINDOW_ROLE"
viewShift = doF . liftM2 (.) W.greedyView W.shift
unfloat = ask >>= doF . W.sink

findAndKill :: Query Bool -> X ()
findAndKill q = raiseAndDo (return ()) q killWindow

-- Spawns a terminal running a command
termWith' p  = myTermFull ++ " -e zsh -ic '" ++ p ++ "'"
-- Stays alive after command exits
termWith p  = termWith' p ++ "'; zsh'"
-- Gives "appName"
namedTermWith' n p = myTermFull ++ " -name " ++ n ++ " -e zsh -ic '" ++ p ++
                                    "'"
namedTermWith  n p = namedTermWith' n p ++ "'; zsh'"
-- }}}2

-- vim: set foldmethod=marker :
