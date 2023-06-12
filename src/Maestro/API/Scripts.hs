module Maestro.API.Scripts where

import           Data.Text            (Text)
import           Maestro.Types.Common (Script)
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
