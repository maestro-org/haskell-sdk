module Maestro.Client.V0.Datum
  ( getDatumByHash
  ) where

import           Data.Text            (Text)
import           Maestro.API.V0       (_datum)
import           Maestro.API.V0.Datum
import           Maestro.Client.Core
import           Maestro.Client.Env
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

datumClient :: MaestroEnv -> DatumAPI (AsClientT IO)
datumClient = fromServant . _datum . apiV0Client

-- | Get information about the datum from it's hash.
getDatumByHash :: MaestroEnv -> Text -> IO Datum
getDatumByHash = _datumByHash . datumClient
