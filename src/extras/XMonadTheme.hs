module XMonadTheme where

data Color = Color
  { cName
  , cNormal
  , cBold   :: String
  } deriving (Show, Eq)

data Theme = Theme
    { tBorder
    , tBorderFocused
    , tForeground
    , tForegroundHi
    , tBackground
    , tBackgroundHi
    , tFont          :: String
    , tFontSize      :: Int
    , tColors        :: [Color]
    } deriving (Show, Eq)

myColors :: [Color]
myColors =
  [ Color "black"   "#282828"   "#454545"
  , Color "red"     "#E93C93"   "#A87181"
  , Color "green"   "#00A17C"   "#76A571"
  , Color "yellow"  "#F79031"   "#D0A845"
  , Color "blue"    "#00C0CE"   "#008BA5"
  , Color "magenta" "#86458B"   "#A0446C"
  , Color "cyan"    "#8EB173"   "#007D62"
  , Color "white"   "#D3CAB3"   "#F4F3E2"
  ]

cHash :: String -> Maybe (String, String)
cHash s = lookup s hmc
  where hmc = map (\(Color n a b) -> (n,(a,b))) myColors
cHash' :: String -> (String, String)
cHash' s = case cHash s of
  Just r  -> r
  Nothing -> error $ "Unable to find color \"" ++ s ++ "\""

myTheme :: Theme
myTheme = Theme
    { tBorder        = "#181512"
    , tBorderFocused = "#E93C93"
    , tForeground    = "#FBF0D2"
    , tForegroundHi  = "#839496"
    , tBackground    = "#181512"
    , tBackgroundHi  = "#86458B"
    , tFont          = "Hack"
    , tFontSize      = 16
    , tColors        = myColors
    }
