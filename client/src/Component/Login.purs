module Component.Login where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE())

import Data.Functor (($>))
import Data.Maybe (Maybe(..))
import Data.Foreign.Generic (Options(), defaultOptions, toJSONGeneric)
import Data.Generic (Generic, gEq, gCompare)

import Network.HTTP.Affjax (AJAX())

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed (InputType(..))
import Halogen.HTML.Events.Types (Event())

import Utils (post)
import Types (Credentials())

data Query a
  = PostCredentials Credentials a

type State = Unit

data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare

type Effects eff = (ajax :: AJAX, console :: CONSOLE | eff)

ui :: forall eff. Component State Query (Aff (Effects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render st =
    H.div_
      [ H.form_
        [ H.input [ P.name "username" ]
        , H.input [ P.name "password", P.inputType InputPassword ]
        , H.button [ E.onClick (\ev -> EH.preventDefault $> (action $ PostCredentials (getCredentials ev))) ] [ H.text "Login" ]
        ]
      ]

  eval :: Eval _ _ _ (Aff (Effects eff))
  eval (PostCredentials cred next) = do
    token <- liftAff' (postCredentials cred)
    pure next

opts :: Options
opts = defaultOptions { unwrapNewtypes = true }

postCredentials :: forall eff. Credentials -> Aff (ajax :: AJAX, console :: CONSOLE | eff) (Maybe String)
postCredentials cred = do
  result <- post "/api/user/login" $ toJSONGeneric opts cred
  log $ toJSONGeneric opts cred
  log $ "post /api/users/login response: " ++ result.response
  return $ Just "test"

foreign import getCredentials :: forall a. Event a -> Credentials
