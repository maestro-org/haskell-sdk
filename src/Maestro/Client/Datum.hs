module Maestro.Client.Datum
  ( getDatumByHash
  ) where

import           Data.Text           (Text)
import           Maestro.API         (_datum)
import           Maestro.API.Datum
import           Maestro.Client
import           Maestro.Client.Env
import           Maestro.Types
import           Servant.API.Generic
import           Servant.Client

datumClient :: MaestroEnv -> DatumAPI (AsClientT IO)
datumClient = fromServant . _datum . apiClient

-- | Get information about the datum from it's hash.
getDatumByHash :: MaestroEnv -> Text -> IO Datum
getDatumByHash = _datumByHash . datumClient
