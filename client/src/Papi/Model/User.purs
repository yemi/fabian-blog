module Papi.Model.User where

import Prelude (class Show)

import Data.Generic (class Generic, gShow)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric)

import Config (genericOptions)

newtype User = User
  { id :: Int
  , username :: String
  , email :: String
  }

derive instance genericUser :: Generic User

instance isForeignUser :: IsForeign User where
  read = readGeneric genericOptions

instance showUser :: Show User where
  show = gShow
