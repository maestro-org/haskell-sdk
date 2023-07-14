module Maestro.Client.V0.Scripts
  ( getScriptByHash
  ) where

import           Data.Text              (Text)
import           Maestro.API.V0         (_scripts)
import           Maestro.API.V0.Scripts
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

scriptsClient :: MaestroEnv 'V0 -> ScriptsAPI (AsClientT IO)
scriptsClient = fromServant . _scripts . apiV0Client

-- | Get information about the script from it's hash.
getScriptByHash :: MaestroEnv 'V0 -> Text -> IO Script
getScriptByHash = _scriptByHash . scriptsClient
