{-# LANGUAGE TemplateHaskell #-}

module MyXmonad.Config.Color where

import Control.Lens hiding (element)
import Data.Prizm.Types

data ThemeColor = ThemeColor
  { _label :: String
  , _norm  :: HexRGB
  , _high  :: HexRGB
  } deriving (Show, Eq)

-- Derived directly from `Nix theme`
data ExtColors = ExtColors
  { _fg          :: ThemeColor
  , _bg          :: ThemeColor
  , _border      :: ThemeColor
  , _focus       :: ThemeColor
  , _cursor      :: ThemeColor
  -- Basic 16 Colors
  , _black       :: ThemeColor
  , _red         :: ThemeColor
  , _green       :: ThemeColor
  , _yellow      :: ThemeColor
  , _blue        :: ThemeColor
  , _magenta     :: ThemeColor
  , _cyan        :: ThemeColor
  , _white       :: ThemeColor
  -- Extendable
  , _extraColors :: [ThemeColor]
  } deriving (Show, Eq)


makeLenses ''ThemeColor
makeLenses ''ExtColors
