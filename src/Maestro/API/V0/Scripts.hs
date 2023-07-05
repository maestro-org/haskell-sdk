module Maestro.API.V0.Scripts where

import           Data.Text           (Text)
import           Maestro.Types.V0
import           Servant.API
import           Servant.API.Generic

newtype ScriptsAPI route =
  ScriptsAPI
    {
      _scriptByHash
        :: route
        :- Capture "script_hash" Text
        :> Get '[JSON] Script
    } deriving Generic
