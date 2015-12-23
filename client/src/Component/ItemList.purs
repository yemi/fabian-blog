module Component.ItemList where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML.Indexed as H

data Query a = Noop a

type State = Unit

data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare

ui :: forall g. Component State Query g
ui = component render eval
  where
    render _ =
      H.div_
        [ H.h1_ [ H.text "This is a list of posts" ]
        , H.p_ [ H.text "swear!" ]
        ]

    eval :: Eval _ _ _ g
    eval (Noop n) = pure n
