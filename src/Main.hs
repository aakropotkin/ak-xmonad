{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
--import MyXmonad.Util (lookupDirEnv)
import XMonad.Layout.Spacing (Border(..), spacingRaw, Spacing)
import XMonad (
  (-->), (|||),
  Choose, Full(..), LayoutClass, ManageHook, Mirror(..), Tall(..), Window,
  XConfig, --Dimension,
  composeAll, def, manageHook, xmonad)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import qualified XMonad as XM
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (
  doCenterFloat, doFullFloat, isFullscreen, isDialog)
import XMonad.Hooks.EwmhDesktops (ewmh)
-- My Modules:
--import MyXmonad.Config as ExC


-- |
-- = Main
main :: IO ()
main = xmonad myConfig


-- These may be overridden in `Main` when environment variables are read.
myConfig :: XConfig MyLayoutHook
myConfig = ewmh def {
    XM.terminal        = ""
  , XM.workspaces      = map show ([1..7] :: [Integer])
                         ++ ["Music", "IRC", "Slack"]
  , XM.startupHook     = return ()
  , XM.layoutHook      = myLayoutHook
  , XM.manageHook      = myManageHook
  --, XM.handleEventHook    = myHandleEventHook
  } --`additionalKeys` keys `removeKeys` myRemoveKeys


-- Modify an existing config by applying settings pulled from ENV VARS.
-- This mostly modifies program names in the keymaps; but it may also apply
-- changes to some logs and hooks.
configFromEnv :: LayoutClass l Window => XConfig l -> IO (XConfig l)
configFromEnv conf = do
  -- Applications
  myTerminal <- lookupEnv "TERMINAL"
  --myBrowser  <- lookupEnv "BROWSER"
  --myEditor   <- lookupEnv "EDITOR"
  --myIRC      <- lookupEnv "IRCCLIENT"
  -- Directories
  --myScrotDir   <- lookupDirEnv "MY_SCROT_DIR"
  return $ conf { XM.terminal = fromMaybe (XM.terminal myConfig) myTerminal }

type MyTiled = ModifiedLayout Spacing Tall
type MyLayoutHook = Choose MyTiled (Choose (Mirror MyTiled) Full)
myLayoutHook :: MyLayoutHook a
myLayoutHook = tiled ||| Mirror tiled ||| Full
  where myGaps     = 5  :: Integer
        border     = Border myGaps myGaps myGaps myGaps
        delta      = 1 / 2
        ratio      = 3 / 100
        tiled      = spacingRaw False border True border True
                                (Tall 1 delta ratio)

myManageHook :: ManageHook
myManageHook = composeAll [
    manageDocks
  , isFullscreen --> doFullFloat
  , isDialog     --> doCenterFloat
  , manageHook def
  ]


{- vim: set foldmethod=marker : -}
