-- | Module to query for /"datum"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/datum).

module Maestro.Client.V1.Datum
  ( getDatumByHash
  ) where

import           Maestro.API.V1         (_datum)
import           Maestro.API.V1.Datum
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.V1
import           Servant.API.Generic
import           Servant.Client

datumClient :: MaestroEnv 'V1 -> DatumAPI (AsClientT IO)
datumClient = fromServant . _datum . apiV1Client

-- | Get information about the datum from it's hash.
getDatumByHash :: MaestroEnv 'V1 -> HexStringOf DatumHash -> IO TimestampedDatum
getDatumByHash = _datumByHash . datumClient
