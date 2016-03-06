module Main where

import           Prelude

import           DOM (DOM())

import           Control.Alt ((<|>))
import           Control.Apply ((*>))
import           Control.Monad.Aff (Aff(), runAff, forkAff)
import           Control.Monad.Aff.AVar (AVAR())
import           Control.Monad.Aff.Console (log)
import           Control.Monad.Eff (Eff())
import           Control.Monad.Eff.Exception (throwException)
import           Control.Monad.Eff.Console (CONSOLE())
import           Control.Monad.Eff.Exception (EXCEPTION())
import           Control.Plus (Plus)

import           Data.Either (Either())
import           Data.Foreign.Generic (Options(), defaultOptions, toJSONGeneric)
import           Data.Functor (($>), (<$))
import           Data.Functor.Coproduct (Coproduct(), left)
import           Data.Maybe (Maybe(..))
import           Data.String (toLower)
import           Data.Tuple (Tuple(..))

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), (:>), cpR, cpL)
import           Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Properties.Indexed as P
import           Halogen.HTML.Properties.Indexed (InputType(..))
import           Halogen.HTML.Events.Types (Event())

import           Network.HTTP.Affjax (AJAX())

import           Routing (matchesAff)
import           Routing.Match (Match())
import           Routing.Match.Class (lit)

import           Types
  ( User()
  , Route(..)
  , AppEffects()
  , State()
  , Query(..)
  )

import qualified Component.Login as Login
import qualified Component.ItemList as ItemList
import qualified Component.Create as Create
import qualified Component.Profile as Profile

-- Child components

type ChildState = Either (Either Login.State ItemList.State) (Either Create.State Profile.State)
type ChildQuery = Coproduct (Coproduct Login.Query ItemList.Query) (Coproduct Create.Query Profile.Query)
type ChildSlot = Either (Either Login.Slot ItemList.Slot) (Either Create.Slot Profile.Slot)

cpLogin :: ChildPath Login.State ChildState Login.Query ChildQuery Login.Slot ChildSlot
cpLogin = cpL :> cpL

cpItemList :: ChildPath ItemList.State ChildState ItemList.Query ChildQuery ItemList.Slot ChildSlot
cpItemList = cpL :> cpR

cpCreate :: ChildPath Create.State ChildState Create.Query ChildQuery Create.Slot ChildSlot
cpCreate = cpR :> cpL

cpProfile :: ChildPath Profile.State ChildState Profile.Query ChildQuery Profile.Slot ChildSlot
cpProfile = cpR :> cpR

-- Root component

type StateP eff = InstalledState State ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

initialState :: State
initialState =
  { isLoading: true
  , user: Nothing
  , currentPage: Login
  }

ui :: forall eff. Component (StateP eff) QueryP (Aff (AppEffects eff))
ui = parentComponent' render eval peek

render :: forall eff. RenderParent State ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
render state =
  H.div_
    [ H.h1_ [ H.text "Admin" ]
    , H.ul_ (map renderLink ["Login", "Posts", "Create", "Profile"])
    , renderPage state.currentPage
    ]

renderLink route = H.li_ [ H.a [ P.href ("#/" ++ toLower route) ] [ H.text route ] ]

renderPage :: forall eff. Route -> HTML (SlotConstructor ChildState ChildQuery (Aff (AppEffects eff)) ChildSlot) Query
renderPage Login = H.slot' cpLogin Login.Slot \_ -> { component: Login.ui, initialState: unit }
renderPage ItemList = H.slot' cpItemList ItemList.Slot \_ -> { component: ItemList.ui, initialState: unit }
renderPage Create = H.slot' cpCreate Create.Slot \_ -> { component: Create.ui, initialState: unit }
renderPage Profile = H.slot' cpProfile Profile.Slot \_ -> { component: Profile.ui, initialState: unit }

eval :: forall eff. EvalParent Query State ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
eval (GoTo page next) = modify (_ { currentPage = page }) $> next

peek :: forall eff. Peek (ChildF ChildSlot ChildQuery) State ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
peek (ChildF p q) = return unit

-- Routing

routing :: Match Route
routing = login <|> posts <|> create <|> profile
  where
    route str = lit "" *> lit str
    login = Login <$ route "login"
    posts = ItemList <$ route "posts"
    create = Create <$ route "create"
    profile = Profile <$ route "profile"

redirects :: forall eff. Driver QueryP eff -> Maybe Route -> Route -> Aff (dom :: DOM, err :: EXCEPTION, avar :: AVAR | eff) Unit
redirects driver _ = driver <<< left <<< action <<< GoTo

routeSignal :: forall eff. Driver QueryP eff -> Aff (dom :: DOM, err :: EXCEPTION, avar :: AVAR | eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

-- Main

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui $ installedState initialState
  onLoad $ appendToBody app.node
  forkAff $ routeSignal app.driver

