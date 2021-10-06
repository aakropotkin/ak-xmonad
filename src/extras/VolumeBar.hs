module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import System.Process
import Control.Monad

pulseVolumePath = "/home/camus/.config/xmonad/src/scripts/pulse-volume.sh"

fps = 60

volumeStep    = 1500.0
defaultVolume = 20000.0
maxVolume     = 90000.0

wWidth  = 300
wHeight = 30
xPos    = 1920 - wWidth - 310
yPos    = 44

window :: Display
window = InWindow "Float" (wWidth, wHeight) (xPos, yPos)

background, foreground, foregroundM :: Color
background  = makeColorI 24 21 18 255
foreground  = makeColorI 233 60 147 255
foregroundM = makeColorI 255 0 0 255

drawing :: VolumeState -> IO Picture
drawing vs = return . scale 0.96 0.95 .
  translate (-fromIntegral wWidth / 2.0) 0 .  color barColor $ polygon
  [ (0, 10), (0, -10), (bWidth, -10), (bWidth, 10) ]
  where bWidth :: Float
        bWidth = volume vs / maxVolume * fromIntegral wWidth
        barColor = if isMuted vs then foregroundM else foreground

data VolumeState = VolumeState {
    volume      :: Float
  , isMuted     :: Bool
  , device      :: String
  , isHeld      :: Bool
  , lastUpdate  :: Float -- In Seconds
}
initialState :: VolumeState
initialState = VolumeState defaultVolume False "Unknown" False 0.0

handleEvent :: Event -> VolumeState -> IO VolumeState
handleEvent e vs' = case e of

  (EventKey k ks m (x, y)) -> case k of

        (Char 'm')               -> do
          when (ks == Down && not (isMuted vs)) $
            callProcess pulseVolumePath [ "mute" ]
          return $ vs { isMuted = True }

        (Char 'u')               -> do
          when (ks == Down && isMuted vs) $
            callProcess pulseVolumePath [ "unmute" ]
          return $ vs { isMuted = False }

        (Char 't')               -> if ks == Up then return vs else do
            callProcess pulseVolumePath [ "toggle" ]
            return $ vs { isMuted = not $ isMuted vs }

        (SpecialKey KeyEsc)      -> exitSuccess

        (MouseButton LeftButton) -> do
          let newVS = updateBar (x, y)
          when (ks == Up) $ callProcess pulseVolumePath
              [ "set", show (round $ volume newVS :: Int) ]
          return $ newVS { isHeld = ks == Down }

        _                       -> return vs

  (EventMotion (x, y))     -> if isHeld vs
    then return $ updateBar (x, y)
    else return vs

  _                        -> return vs

  where vs = vs' { lastUpdate = 0.0 }
        updateBar :: (Float, Float) -> VolumeState
        updateBar (x, y) = 
          let (ix, iy) = (x / 0.96 + fromIntegral wWidth / 2, y / 0.95)
              xPerc    = min 1.0 $ max 0.0 $ ix / fromIntegral wWidth
              newVol   = maxVolume * xPerc
              newMute  = newVol == 0
          in vs { volume = newVol, isMuted = newMute }

updateState :: Float -> VolumeState -> IO VolumeState
updateState f vs = if lastUpdate vs > 5.0
  then do
    exitSuccess
    return vs
  else return $ vs { lastUpdate = lastUpdate vs + f }

main :: IO ()
main = do
  dev  <- readFile "/tmp/.soundout"
  vol  <- min maxVolume . max 0 . read <$> readFile "/tmp/.volume"
  mute <- (/=) 0 . read <$> readFile "/tmp/.mute"
  let is = initialState { volume = vol, device = dev, isMuted = mute }
  playIO window background fps is drawing handleEvent updateState
