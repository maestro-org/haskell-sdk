module Maestro.Client.Scripts
  ( getScriptByHash
  ) where

import           Data.Text           (Text)
import           Maestro.API         (_scripts)
import           Maestro.API.Scripts
import           Maestro.Client.Core
import           Maestro.Client.Env
import           Maestro.Types
import           Servant.API.Generic
import           Servant.Client

scriptsClient :: MaestroEnv -> ScriptsAPI (AsClientT IO)
scriptsClient = fromServant . _scripts . apiClient

-- | Get information about the script from it's hash.
getScriptByHash :: MaestroEnv -> Text -> IO Script
getScriptByHash = _scriptByHash . scriptsClient
