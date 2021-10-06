module SpotInfo where

import Control.Monad.IO.Class
import DBus
import DBus.Client
import Data.Maybe
import Control.Arrow ((***))

-- FIXME: There is some seriously fucky behavior with Variants
--        I had to do this super dirty by printing them and parsing the output
--        rather than actually using the object.
--        I think it gets pissed off when you try to bind it and then use it.
--        Most likely it needs to operate in the same monadic instance.
--        (It is likely carrying important type information in an environment)

spr = do
  client <- connectSession
  p <- call_ client (methodCall
    (objectPath_ "/org/mpris/MediaPlayer2")
    (interfaceName_ "org.freedesktop.DBus.Properties")
    (memberName_ "Get"))
      { 
        methodCallBody = [
            toVariant "org.mpris.MediaPlayer2.Player"
          , toVariant "Metadata"
          ]
        , methodCallDestination = Just (busName_ "org.mpris.MediaPlayer2.spotify")
      }
  disconnect client
  let m = (map (fromVar' *** fromVar') . unpack .  head . methodReturnBody) p
      artist = init $ init $ drop 10 $ fromJust $ lookup "xesam:albumArtist" m
      album  = init $ drop 9 $ fromJust $ lookup "xesam:album" m
      track  = init $ drop 9 $ fromJust $ lookup "xesam:title" m
  putStrLn track
  putStrLn $ "By, " ++ artist
  putStrLn $ "On, " ++ album
  return ()
  where unpack v = case variantType v of
          TypeDictionary _ _ -> dictionaryItems $ fromVar v
          TypeVariant -> unpack $ fromVar v
          TypeStructure _ ->
            let x = structureItems (fromVar v) in
              if null x then [] else unpack (head x)
          _ -> []
        fromVar :: IsVariant a => Variant -> a
        fromVar = fromJust . fromVariant
        fromVar' :: Variant -> String
        fromVar' = init . drop 9 . show
