module Cool where

import Prelude

import Effect (Effect)
import Erl.Data.List (List)

foreign import data Pony :: Type

foreign import createPony :: String -> Pony
foreign import areAnyPoniesNotAwesome :: List Pony -> Boolean
foreign import writePoniesToFile :: String -> List Pony -> Effect Boolean
