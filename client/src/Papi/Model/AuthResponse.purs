module Papi.Model.AuthResponse where

import Prelude (class Show)

import Data.Generic (class Generic, gShow)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric)

import Papi.Model.User (User)

import Config (genericOptions)

newtype AuthResponse = AuthResponse
  { jwt :: String
  , user :: User
  }

derive instance genericAuthResponse :: Generic AuthResponse

instance isForeignAuthResponse :: IsForeign AuthResponse where
  read = readGeneric genericOptions

instance showAuthResponse :: Show AuthResponse where
  show = gShow
