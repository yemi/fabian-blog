module Types where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Console (CONSOLE())

import Data.Maybe (Maybe())
import Data.Either (Either())
import Data.Generic (Generic)
import Data.Functor.Coproduct (Coproduct())

import Halogen (HalogenEffects(), InstalledState(), ChildF())

import Network.HTTP.Affjax (AJAX())

newtype User = User
  { username :: String
  }

newtype Credentials = Credentials
  { username :: String
  , password :: String
  }

data Route
  = Login
  | ItemList
  | Create
  | Profile

data Query a = GoTo Route a

type State =
  { isLoading :: Boolean
  , user :: Maybe User
  , currentPage :: Route
  }

type AppEffects eff = HalogenEffects 
  ( ajax :: AJAX
  , console :: CONSOLE
  | eff )

derive instance genericCredentials :: Generic Credentials
