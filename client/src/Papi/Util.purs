module Papi.Util where

import Halogen.HTML.Events.Types (Event())
import Papi.Model.LoginCredentials (LoginCredentials())

foreign import getLoginCredentials :: forall a. Event a -> LoginCredentials
