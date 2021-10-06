module Main where


{- Imports: ---------------------------------------------------------- {{{1 -}
import Xmobar
import XMonadTheme
import System.Process
import System.Environment
import System.Posix.User
import Graphics.X11.Xinerama
import Graphics.X11.Xlib.Display
{- End Imports ------------------------------------------------------- }}}1 -}

{- Default Config: --------------------------------------------------- {{{1 -}
defConf = Config
  { wmName            = "xmobar"
  , wmClass           = "xmobar"
  , font              = "xft:" ++ tFont myTheme ++ ":size=" ++
                          show ( tFontSize myTheme)
  , additionalFonts   = [ "xft:" ++ tFont myTheme ]
  , bgColor           = tBackground myTheme
  , fgColor           = tForeground myTheme
  , borderColor       = tBackgroundHi myTheme
  , borderWidth       = 3
  , border            = BottomB
  , alpha             = 200
  , textOffset        = -1
  , textOffsets       = []
  , iconOffset        = -1
  , lowerOnStart      = True
  , pickBroadest      = False
  , persistent        = False
  , hideOnStart       = False
  , iconRoot          = "."
  , allDesktops       = True
  , overrideRedirect  = True
  , sepChar           = "%"
  , alignSep          = "}{"
  , verbose           = False
  , position = Static { xpos = 0, ypos = 0, width = 1920, height = 42 }
  , template          = " %StdinReader% }{ "
  , commands          = [ Run StdinReader ]
  }
{- End Default Config ------------------------------------------------ }}}1 -}

{- Main: ------------------------------------------------------------- {{{1 -}
main = do
  args <- getArgs
  machine <- init <$> readProcess "hostname" [] ""
  case machine of
    "angela" -> case head args of
                  "left"    -> xmobar angelaLeftBar
                  "center"  -> xmobar angelaCenterBar
                  "right"   -> xmobar angelaRightBar
                  a         -> putStrLn $ "angela has no bar \"" ++ a ++ "\""
    "alonso" -> case head args of
                  "left"    -> xmobar angelaLeftBar
                  "center"  -> xmobar angelaCenterBar
                  "right"   -> xmobar angelaRightBar
                  a         -> putStrLn $ "alonso has no bar \"" ++ a ++ "\""
    "petr"  -> if "dual" `elem` args
                then do
                      putStrLn "Launching Second Bar in Dual Mode."
                      xmobar petrBar { position = Static {
                        xpos = 1920, ypos = 90, width = 1600, height = 42 } }
                else xmobar petrBar
    "kant"  -> xmobar kantBar { position = Static {
                xpos = 0, ypos = 0, width = 1366, height = 42 } }
    _ -> do
      user <- getLoginName
      case user of
        "badalex" -> xmobar marxBar
        _         -> putStrLn $ "Cannot find config for \"" ++ user ++
                      '@':machine ++ "\""

scriptDir = "/home/camus/.config/xmonad/src/scripts"

foreHi  = tForegroundHi myTheme
black   = fst $ cHash' "black"
red     = fst $ cHash' "red"
green   = fst $ cHash' "green"
yellow  = fst $ cHash' "yellow"
yellowB = snd $ cHash' "yellow"
blue    = fst $ cHash' "blue"
blueB   = snd $ cHash' "blue"
magenta = fst $ cHash' "magenta"
cyan    = fst $ cHash' "cyan"
white   = fst $ cHash' "white"
sep     = hlit "|"
sep'    = "}{"

colorize :: String -> String -> String
colorize col str = "<fc=" ++ col ++ ('>':str) ++ "</fc>"

hlit :: String -> String
hlit = colorize foreHi

{- End Main ---------------------------------------------------------- }}}1 -}

{- Commands: --------------------------------------------------------- {{{1 -}

data AudioToggle = AudioToggle deriving (Read, Show)
instance Exec AudioToggle where
  alias AudioToggle = "AudioToggle"
  run AudioToggle = return $ "<action=`" ++ scriptDir ++
    "/pulse-switch.sh toggle`>%soundout%</action>"

myBattery = Run $ BatteryP ["BAT0"] [
    "-t", (hlit "Bat:") ++ " <acstatus> (<left>%)"
  , "-L", "10", "-H", "80", "-p", "3"
  , "--"
  , "-i", colorize green  "Full"
  , "-O", colorize blue   "On"
  , "-o", colorize yellow "Off"
  , "-L", "-15"
  , "-H", "-5"
  , "-l", red
  , "-m", blue
  , "-h", green
  ] 600

myBatteryMin = Run $ BatteryP ["BAT0"] [
    "-t", "<acstatus><left>%</fc>"
  , "--"
  , "-i", "<fc=" ++ green ++ ">"    --Full
  , "-O", "<fc=" ++ blue ++ ">"     --On
  , "-o", "<fc=" ++ yellow ++ ">"   --Off
  ] 600

myWeather = Run $ Weather "KAUS" [
    "-t",       (hlit "ATX:") ++ "<tempC>°C"
  , "-L",       "18"
  , "-H",       "25"
  , "--normal", green
  , "--high",   red
  , "--low",    blueB
  ] 600

myCPU = Run $ Cpu [
    "-t",       (hlit "Cpu:") ++ "<total>%"
  , "-L",       "3"
  , "-H",       "50"
  , "--normal", green
  , "--high",   red
  ] 10

myMemory = Run $ Memory [
  "-t", (hlit "Mem:") ++ " <usedratio>%"
  ] 10

mySwap = Run $ Swap [
  "-t", (hlit "Swap:") ++ " <usedratio>%"
  ] 10

myNet n = Run $ Network n [
    "-t",       (hlit "<dev>:") ++ " <rx>KB " ++ sep ++ "<tx>KB"
  , "-L",       "0"
  , "-H",       "32"
  , "--normal", green
  , "--high",   red
  ] 10

myNetMin n = Run $ Network n [
    "-t",       (hlit "<dev>:") ++ " <rx>KB " ++ sep ++ "<tx>KB"
  , "-L",       "0"
  , "-H",       "32"
  , "--normal", green
  , "--high",   red
  ] 10

--getScreenRect :: Int -> IO Rectangle
getScreenRect n = do
  dpy <- openDisplay ""
  srs <- getScreenInfo dpy
  return $ srs !! n

--getScreenRect n = openDisplay "" >>= getScreenInfo >>= 


calAct g = "<action=`xmessage \"$(cal)\" -title Calendar " ++
  "-geometry " ++ g ++ "`>" ++
  (colorize yellow "%day%") ++ "</action>"

timeTemp = ' ':(hlit "%time%")

dateTimeTemp g = " " ++ calAct g ++ " " ++ sep ++ timeTemp

myVolume = Run $ Volume "default" "Master" [
    "-t", (hlit "Vol:") ++ " <volume>% <status>"
  , "--"
  , "-c", magenta
  , "-C", green
  ] 10

myVolumeScripts = [
    Run $ Com (scriptDir ++ "/pulse-volume.sh") ["read"] "volume"   10
  , Run $ Com (scriptDir ++ "/pulse-switch.sh") ["read"] "soundout" 10
  ]

myMarque = Run $ MarqueePipeReader "/tmp/.barmessages" (120, 5, " : ") "mpipe"

myCoreTemp = Run $ CoreTemp [
    "-t", (hlit ",") ++ "<core0>°C"
  , "-L", "50"
  , "-H", "70"
  , "-l", blueB
  , "-n", foreHi
  , "-h", red
  ] 50

{- End Commands ------------------------------------------------------ }}}1 -}


{- Angela's Bars: ---------------------------------------------------- {{{1 -}

angelaLeftBar, angelaCenterBar, angelaRightBar, petrBar :: Config

angelaPosition = TopSize C 100 42

angelaLeftBar = defConf {
    commands = [ Run StdinReader ] ++ myVolumeScripts
  --, position = Static { xpos = 0, ypos = 0, width = 1080, height = 42 }
  , position = OnScreen 0 angelaPosition
  , template = " %StdinReader% " ++ sep' ++
               volAct            ++ sep ++
               " <action=`volumebar`>" ++ (hlit "Vol:") ++ " %volume%</action> "
  } where volAct = " <action=`" ++ scriptDir ++ "/pulse-switch.sh toggle`>"
                    ++ "%soundout%</action> "

angelaCenterBar = defConf {
    wmName   = "xmobar-center"
  , commands = [ myNet "enp2s0" , myCPU , myMemory , mySwap, myMarque ]
  --, position  = Static { xpos = 1080, ypos = 0, width = 1920, height = 42 }
  , position = OnScreen 1 angelaPosition
  , template = " %cpu% " ++ sep ++ " %memory% * %swap% " ++ sep ++ sep'
                ++ "     %enp2s0% "
  }

angelaRightBar = defConf {
    wmName   = "xmobar-right"
  , template = " %KAUS% " ++ sep' ++
               dateTimeTemp "273x195+5000+43"
  , commands = [ myWeather
               , Run $ Date "%I:%M:%S"      "time"  10
               , Run $ Date "%a %b %_d %Y"  "day"   30
               ]
  --, position = Static { xpos = 3000, ypos = 0, width = 1080, height = 42 }
  , position = OnScreen 2 angelaPosition
  }
{- End Angela's Bars ------------------------------------------------- }}}1 -}

{- ======================================================================== -}

{- Petr's Bars: ------------------------------------------------------ {{{1 -}

petrBar = defConf {
    template = " %StdinReader%"                 ++ sep ++
                (hlit "Bat:") ++ "%battery%"    ++ sep ++
                "%bright%"                      ++ sep ++
                "%cpu%"                         ++
                "%coretemp%"                    ++ sep
                                                ++ sep' ++
                petrVolumeTemplate              ++ sep ++
                "%KAUS%"                        ++ sep ++
                dateTimeTemp "273x195+1640+43"  ++ " "

  , commands = [ Run  StdinReader

               , Run $ Com (scriptDir ++ "/pulse-volume.sh") [ "read" ]
                  "volume" 10
               , Run $ Com (scriptDir ++ "/pulse-switch.sh") [ "read" ]
                  "soundout" 10

               , Run $ Date "%I:%M:%S" "time" 10
               , Run $ Date "%a %b %_d" "day" 30

               , Run $ Brightness [
                    "-t", (hlit "Hi:") ++ "<percent>%"
                  , "--"
                  , "-D", "/sys/class/backlight/intel_backlight"
                  ] 60

               , myBatteryMin, myWeather, myCPU, myCoreTemp
               ]
  } where petrVolumeTemplate = "<action=`volumebar`>" ++ (hlit "V:") ++
            "%volume%</action>"

{- End Petr's Bars --------------------------------------------------- }}}1 -}

{- ======================================================================== -}

{- Kant's Bars: ------------------------------------------------------ {{{1 -}

kantBar = defConf {
    template = " %StdinReader% "    ++ sep ++
               " %battery% "        ++ sep' ++
               " %default:Master% " ++ sep ++
               " %KAUS% "           ++ sep ++
               dateTimeTemp "273x195+1640+43"

  , commands = [ Run StdinReader
               , Run $ Com (scriptDir ++ "/pulse-switch.sh") ["read"]
                  "soundout" 10
               , Run $ Date "%I:%M:%S" "time" 10
               , Run $ Date "%a %b %_d %Y" "day" 30
               , myVolume, myWeather, myBattery
               ]
  }
{- End Kant's Bars --------------------------------------------------- }}}1 -}

{- Marx's Bars: ------------------------------------------------------ {{{1 -}
marxBar = defConf {
    template  = " %StdinReader% " ++ sep' ++ " %default:Master% " ++ sep ++
        " %KAUS% " ++ sep ++ dateTimeTemp "273x195+1640+43"
  , commands = [ Run StdinReader
               , Run $ Date "%I:%M:%S" "time" 10
               , Run $ Date "%a %b %_d %Y" "day" 30
               , myVolume, myWeather
               ]
  }
{- End Marx's Bars --------------------------------------------------- }}}1 -}


{- vim: set foldmethod=marker : -}
