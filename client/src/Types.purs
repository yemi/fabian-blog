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

-- Routing

data Route
  = Login
  | ItemList
  | Create
  | Profile

-- Effects

type AppEffects eff = HalogenEffects
  ( ajax :: AJAX
  , console :: CONSOLE
  , webStorage :: WebStorage
  | eff )

