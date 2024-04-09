-- | Module to query for /\"Assets\"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/Indexer-API/Assets/asset-addresses).
module Maestro.Client.V1.Assets (
  assetInfo,
) where

import Maestro.API.V1 (assets)
import qualified Maestro.API.V1.Assets as Mapi
import Maestro.Client.Env
import Maestro.Client.V1.Core
import Maestro.Types.V1
import Servant.API.Generic
import Servant.Client

txClient :: MaestroEnv 'V1 -> Mapi.AssetsAPI (AsClientT IO)
txClient = fromServant . assets . apiV1Client

-- | Native asset information.
assetInfo ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | `NonAdaNativeToken` to query.
  NonAdaNativeToken ->
  IO TimestampedAssetInfo
assetInfo = Mapi.assetInfo . txClient
