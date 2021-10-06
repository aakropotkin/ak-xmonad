
{- Setup: ------------------------------------------------------------- {{{1 -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- module MyXmonad.Config (
--   ExtConfig (..),
--   ExtTheme  (..),
--   ExtProgs  (..),
--   ExtDirs   (..),
--   ExtPaths  (..),
--   ExtColors (..),
--   ExtPretty (..),
--   ExtDims   (..),
--   readExtDirs
-- ) where

module MyXmonad.Config where

import Data.Map (Map, empty)
import Data.Maybe (fromJust)
import Control.Lens hiding (element)
import XMonad.Hooks.DynamicLog (PP)
import XMonad
-- My Modules:
import MyXmonad.Config.Color
import MyXmonad.Util
{- End Setup ---------------------------------------------------------- }}}1 -}

{- ========================================================================= -}

{- Top: --------------------------------------------------------------- {{{1 -}
data ExtConfig = ExtConfig
  { _extPaths     :: ExtPaths
  , _extDirs      :: ExtDirs
  , _extProgs     :: ExtProgs
  , _extTheme     :: ExtTheme
  } deriving (Show, Eq)
{- End Top ------------------------------------------------------------ }}}1 -}

{- Layer 2: ----------------------------------------------------------- {{{1 -}
data ExtTheme = ExtTheme
  { _colors       :: ExtColors            -- ^ See `Config.Colors`
  , _fonts        :: ExtFonts
  , _dimensions   :: ExtDims
  , _pretties     :: ExtPretty
  , _extraThemes  :: Map String String
  } deriving (Show, Eq)

data ExtProgs = ExtProgs
  { _terminal     :: String
  , _shell        :: String
  , _pager        :: String
  , _browser      :: String
  , _editor       :: String
  , _irc          :: String
  , _monitor      :: String
  , _volumeMgr    :: String
  , _extraProgs   :: Map String String
  } deriving (Show, Eq)

data ExtDirs = ExtDirs
  { _homeDir      :: FilePath
  , _nixOSDir     :: FilePath
  , _xdgConfDir   :: FilePath
  , _nixHomeDir   :: FilePath
  , _zshDir       :: FilePath
  , _vimDir       :: FilePath
  , _shareDir     :: FilePath
  , _xmoConfDir   :: FilePath
  , _scrotDir     :: FilePath
  , _docsDir      :: FilePath
  , _srcDir       :: FilePath
  , _downDir      :: FilePath
  , _extraDirs    :: Map String FilePath
  } deriving (Show, Eq)

data ExtPaths = ExtPaths
  { _homeRC       :: FilePath
  , _vimRC        :: FilePath
  , _zshRC        :: FilePath
  , _nixOSConf    :: FilePath
  , _nixPaths     :: [FilePath]
  , _extraPaths   :: Map String FilePath
  } deriving (Show, Eq)
{- End Layer 2 -------------------------------------------------------- }}}1 -}

{- Layer 3: ----------------------------------------------------------- {{{1 -}

-- (Name, Size)
data ExtFonts = ExtFonts
  { _promptFont   :: Font
  , _barFont      :: Font
  , _extraFonts   :: Map String Font
  } deriving (Show, Eq)

data ExtDims = ExtDims
  { _gaps         :: Integer
  , _winBorder    :: Dimension

  , _barHeight    :: Int
  , _barBorder    :: Int

  , _promptHeight :: Dimension
  , _promptBorder :: Dimension

  , _extraDims    :: Map String Dimension
  } deriving (Show, Eq)

data ExtPretty = ExtPretty
  { _barsPP       :: PP
  , _extraPretty  :: Map String PP
  }
instance Show ExtPretty where
  show (ExtPretty {..}) = ""
instance Eq ExtPretty where
  x == y = False
{- End Layer 3 -------------------------------------------------------- }}}1 -}

readExtDirs :: IO ExtDirs
readExtDirs =
  let envVars = [
          "HOME"
        , "MY_NixOS_DIR"
        , "MY_CONF_DIR"
        , "MY_NIX_HOME_DIR"
        , "MY_ZSH_DIR"
        , "MY_VIM_DIR"
        , "MY_SHARE_DIR"
        , "MY_XMO_CONF_DIR"
        , "MY_SCROT_DIR"
        ]
      ioVars = sequence $ map (\v -> fromJust <$> (lookupDirEnv v)) envVars
  in do
    vars <- ioVars
    let home  = head vars
        docs  = home <> "/docs"
        src   = home <> "/src"
        down  = home <> "/down"
        extra = empty
    return $ ExtDirs home (vars !! 1) (vars !! 2) (vars !! 3) (vars !! 4)
      (vars !! 5) (vars !! 6) (vars !! 7) (vars !! 8) docs src down extra

{- NOTE:
  ```ghci
  > import Control.Lens
  > import MyXmonad
  > ed <- readExtDirs
  > ed ^. homeDirs
  "/home/camus"
  it :: FilePath
  ```
-}

{- Templates: --------------------------------------------------------- {{{1 -}
-- Layer 3
makeLenses ''ExtPretty
makeLenses ''ExtDims
makeLenses ''ExtFonts
-- Layer 2
makeLenses ''ExtPaths
makeLenses ''ExtDirs
makeLenses ''ExtProgs
makeLenses ''ExtTheme
-- Top
makeLenses ''ExtConfig
{- End Templates ------------------------------------------------------ }}}1 -}

{- ========================================================================= -}

{- vim: set foldmethod=marker : -}
