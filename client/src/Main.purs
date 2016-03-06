module Main where

import Control.Monad.Aff (runAff, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throwException)

import Halogen (runUI)
import Halogen.Util (appendToBody, onLoad)

import Prelude (Unit, ($), bind, unit, pure, const)

import Papi (ui, initialState)

import Router (routeSignal)

import Types (AppEffects)

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
  forkAff $ routeSignal app.driver

