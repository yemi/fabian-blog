module Component.Login where

import           Prelude

import           Browser.WebStorage (WebStorage, localStorage, setItem)

import           Control.Monad.Aff (Aff())
import           Control.Monad.Aff.Console (log)
import           Control.Monad.Eff.Class (liftEff)
import           Control.Monad.Eff.Console (CONSOLE())

import           Data.Foreign (F())
import           Data.Foreign.Class (readJSON)
import           Data.Foreign.Generic (Options(), defaultOptions, toJSONGeneric)
import           Data.Functor (($>))
import           Data.Generic (Generic, gEq, gCompare)
import           Data.Maybe (Maybe(..))
import           Data.Either (Either(..))

import           Network.HTTP.Affjax (Affjax(), AJAX(), AffjaxResponse())
import           Network.HTTP.StatusCode (StatusCode(..))

import           Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Properties.Indexed as P
import           Halogen.HTML.Properties.Indexed (InputType(..))
import           Halogen.HTML.Events.Types (Event())

import           Utils (post)
import           Types (Credentials(), AuthResponse(..))

import           Config (genericOptions)

data Query a
  = Login Credentials a

type State = Unit

data Slot = Slot

derive instance slotGeneric :: Generic Slot
instance eqSlot :: Eq Slot where eq = gEq
instance ordSlot :: Ord Slot where compare = gCompare

type Effects eff = (ajax :: AJAX, console :: CONSOLE, webStorage :: WebStorage | eff)

ui :: forall eff. Component State Query (Aff (Effects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render st =
    H.div_
      [ H.form_
        [ H.input [ P.name "username" ]
        , H.input [ P.name "password", P.inputType InputPassword ]
        , H.button [ E.onClick (\ev -> EH.preventDefault $> (action $ Login (getCredentials ev))) ]
          [ H.text "Login" ]
        ]
      ]

  eval :: Eval _ _ _ (Aff (Effects eff))
  eval (Login cred next) = do
    { status, response } <- liftAff' $ postCredentials cred

    -- Save JWT to localStorage on successful login
    if status == StatusCode 201
      then
        case readJSON response of
          Left _ -> return unit
          Right (AuthResponse { jwt, user }) -> do
            liftAff' $ log $ "Logged in user: \n" ++ show user
            liftEff' $ setItem localStorage "fabian-blog-jwt" jwt
      else do
        liftAff' $ log "Login failed"
        return unit

    return next

postCredentials :: forall eff. Credentials -> Affjax eff String
postCredentials = post "/api/users/login" <<< toJSONGeneric genericOptions

foreign import getCredentials :: forall a. Event a -> Credentials

