module Main where

import Prelude

import Cool (areAnyPoniesNotAwesome)
import Cool as Cool
import Effect (Effect)
import Effect.Console (log) as Console
import Erl.Data.List (nil, (:))

foreign import base64 :: String -> String

add :: Int -> Int -> Int
add x y = x + y

somethingCool :: Effect Unit
somethingCool = do
  let ponies = Cool.createPony "Pinkie Pie"
             : Cool.createPony "Rarity"
             : nil
  let notAwesome = Cool.areAnyPoniesNotAwesome ponies
  if notAwesome then do
    _ <- Console.log "Sadness"
    pure unit
  else do
    _ <- Console.log "Yay"
    result <- Cool.writePoniesToFile "file.txt" ponies
    pure unit
