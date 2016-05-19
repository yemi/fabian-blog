module Config where

import Data.Foreign.Generic (Options(), defaultOptions)

genericOptions :: Options
genericOptions = defaultOptions { unwrapNewtypes = true }

localStorageKey :: String
localStorageKey = "papi-auth-token"

