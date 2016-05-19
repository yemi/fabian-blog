module Types where

import Browser.WebStorage (WebStorage)

import Control.Monad.Eff.Console (CONSOLE)

import Halogen (HalogenEffects)

import Network.HTTP.Affjax (AJAX)

-- Routing

data Route
  = Login
  | ItemList
  | Create
  | Profile
  | Dashboard

-- Effects

type AppEffects eff = HalogenEffects
  ( ajax :: AJAX
  , console :: CONSOLE
  , webStorage :: WebStorage
  | eff )

