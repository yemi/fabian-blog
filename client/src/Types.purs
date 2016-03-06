module Types where

import Prelude

import Browser.WebStorage (WebStorage())

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Console (CONSOLE())

import Data.Either (Either())
import Data.Foreign.Class (IsForeign, readProp)
import Data.Foreign.Generic (readGeneric)
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gShow)
import Data.Maybe (Maybe())

import Halogen (HalogenEffects(), InstalledState(), ChildF())

import Network.HTTP.Affjax (AJAX())

import Config (genericOptions)

-- User

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

-- Authorization

newtype Credentials = Credentials
  { username :: String
  , password :: String
  }

derive instance genericCredentials :: Generic Credentials

newtype AuthResponse = AuthResponse
  { jwt :: String
  , user :: User
  }

derive instance genericAuthResponse :: Generic AuthResponse

instance isForeignAuthResponse :: IsForeign AuthResponse where
  read = readGeneric genericOptions

instance showAuthResponse :: Show AuthResponse where
  show = gShow

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
  , webStorage :: WebStorage
  | eff )

