module Papi.State where

import Data.Maybe (Maybe(..))

import Papi.Model (User)

import Types (Route(..))

type State =
  { isLoading :: Boolean
  , user :: Maybe User
  , authToken :: Maybe String
  , currentPage :: Route
  }

initialState :: State
initialState =
  { isLoading: true
  , user: Nothing
  , authToken: Nothing
  , currentPage: Login
  }

