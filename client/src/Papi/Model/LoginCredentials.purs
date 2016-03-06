module Papi.Model.LoginCredentials where

import Data.Generic (Generic)

newtype LoginCredentials = LoginCredentials
  { username :: String
  , password :: String
  }

derive instance genericLoginCredentials :: Generic LoginCredentials
