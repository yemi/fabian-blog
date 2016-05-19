module Papi.Query where

import Papi.Model(LoginCredentials)

import Types (Route)

data Query a
  = GoTo Route a
  | Authenticate LoginCredentials a
