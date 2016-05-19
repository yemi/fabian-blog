module Router where

import Prelude (Unit, bind, (<<<))

import DOM (DOM)

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Functor ((<$))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

import Halogen (Driver, action)

import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit)

import Types (Route(..))

import Papi.Query (Query(GoTo))

routing :: Match Route
routing = login <|> posts <|> create <|> profile
  where
    route str = lit "" *> lit str
    login = Login <$ route "login"
    posts = ItemList <$ route "posts"
    create = Create <$ route "create"
    profile = Profile <$ route "profile"

redirects :: forall eff. Driver Query eff -> Maybe Route -> Route -> Aff (dom :: DOM, err :: EXCEPTION, avar :: AVAR | eff) Unit
redirects driver _ = driver <<< action <<< GoTo

routeSignal :: forall eff. Driver Query eff -> Aff (dom :: DOM, err :: EXCEPTION, avar :: AVAR | eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new
