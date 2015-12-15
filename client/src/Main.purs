module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Console (CONSOLE())

import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Foreign.Class (readProp)
import Data.Maybe (Maybe(..))
import Data.Generic
import Data.Foreign.Generic (Options(), defaultOptions, toJSONGeneric)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed (InputType(..))
import Halogen.HTML.Events.Types (Event())

import Network.HTTP.Affjax (AJAX(), URL(), Affjax(), affjax, defaultRequest)
import Network.HTTP.Method (Method(..))
import Network.HTTP.Affjax.Request (Requestable)
import Network.HTTP.Affjax.Response (Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))

foreign import getCredentials :: forall a. Event a -> Credentials

type User =
  { username :: String
  }

newtype Credentials = Credentials
  { username :: String
  , password :: String
  }

derive instance genericCredentials :: Generic Credentials

-- | The state of the component.
type State =
  { isLoading :: Boolean
  , user :: Maybe User
  }


initialState :: State
initialState =
  { isLoading: true
  , user: Nothing
  }

-- | The component query algebra.
data Query a
  = Login Credentials a

-- | The effects used in the app.
type AppEffects eff = HalogenEffects (ajax :: AJAX, console :: CONSOLE | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render st =
    H.div_
      [ H.form_
        [ H.input [ P.name "username" ]
        , H.input [ P.name "password", P.inputType InputPassword ]
        , H.button [ E.onClick (\ev -> EH.preventDefault $> (action $ Login (getCredentials ev))) ] [ H.text "Login" ]
        ]
      ]

  eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
  --eval (SetCode code next) = modify (_ { code = code, result = Nothing :: Maybe String }) $> next
  eval (Login cred next) = do
    modify (_ { isLoading = true })
    token <- liftAff' (login cred)
    modify (_ { isLoading = false })
    pure next

-- | Post some PureScript code to the trypurescript API and fetch the JS result.
fetchJS :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchJS code = do
  result <- post "http://try.purescript.org/compile/text" code
  let response = result.response
  return case readProp "js" response <|> readProp "error" response of
    Right js -> js
    Left _ -> "Invalid response"

opts :: Options
opts = defaultOptions { unwrapNewtypes = true }

post :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax e b
post url content = affjax $ defaultRequest
  { method = POST
  , url = url
  , content = Just content
  , headers = [ RequestHeader "Accept" "application/json", RequestHeader "Content-Type" "application/json" ]
  }

login :: forall eff. Credentials -> Aff (ajax :: AJAX, console :: CONSOLE | eff) (Maybe String)
login cred = do
  result <- post "/api/user/login" $ toJSONGeneric opts cred
  log $ toJSONGeneric opts cred
  log $ "post /api/users/login response: " ++ result.response
  return $ Just "test"

-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
