module Papi where

import Browser.WebStorage as WS

import Config (genericOptions, localStorageKey)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)

import Data.Either (Either(..))
import Data.Foreign.Generic (toJSONGeneric)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.Foreign.Class (readJSON)

import Halogen (Eval, ComponentHTML, Component, liftAff', liftEff', modify, action, component)
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (InputType(..))
import Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (Affjax)
import Network.HTTP.StatusCode (StatusCode(..))

import Papi.Model (LoginCredentials, User(..), AuthResponse(..))
import Papi.Util (getLoginCredentials)
import Papi.Api.Util (post)
import Papi.State (State)
import Papi.Query (Query(..))

import Prelude ((<<<), return, unit, ($), bind, show, (++), (==), map)

import Types (Route(..), AppEffects)

ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = component render eval

render :: State -> ComponentHTML Query
render state =
  H.div_
    [ H.h1_ [ H.text "Admin" ]
    , H.ul_ (map renderLink ["Login", "Posts", "Create", "Profile"])
    , renderPage state.currentPage state
    ]

renderLink :: String -> ComponentHTML Query
renderLink route = H.li_ [ H.a [ P.href ("#/" ++ toLower route) ] [ H.text route ] ]

renderPage :: Route -> State -> ComponentHTML Query
renderPage Login state =
  H.div_
    [ H.form_
      [ H.input [ P.name "username" ]
      , H.input [ P.name "password", P.inputType InputPassword ]
      , H.button [ E.onClick (\ev -> EH.preventDefault $> (action $ Authenticate (getLoginCredentials ev))) ]
        [ H.text "Login" ]
      ]
    ]

renderPage Dashboard state =
  H.div_
    [
      H.text "Hello dashboard, "
    ]

renderPage _ _ = H.div_ [ H.text "Nuru" ]

eval :: forall eff. Eval Query State Query (Aff (AppEffects eff))
eval (GoTo route next) = modify (_ { currentPage = route }) $> next
eval (Authenticate cred next) = do
  { status, response } <- liftAff' $ postCredentials cred

  -- Save JWT to localStorage on successful login
  if status == StatusCode 201
    then
      case readJSON response of
        Left _ -> return unit
        Right (AuthResponse { jwt, user }) -> do
          liftAff' $ log $ "Logged in user: \n" ++ show user
          liftEff' $ WS.setItem WS.localStorage localStorageKey jwt
          modify (_ { authToken = Just jwt, user = Just user})
          eval (GoTo Dashboard unit)
    else do
      liftAff' $ log "Login failed"
      return unit

  return next

postCredentials :: forall eff. LoginCredentials -> Affjax eff String
postCredentials = post "/api/users/login" <<< toJSONGeneric genericOptions

