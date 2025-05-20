module Maestro.API.V1.Assets where

import           Maestro.Client.V1.Core.Pagination
import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data AssetsAPI route = AssetsAPI
  { assetInfo ::
      route
        :- Capture "asset" NonAdaNativeToken
          :> Get '[JSON] TimestampedAssetInfo
  -- ^ Native asset information.
  , assetUTxOs ::
      route
        :- Capture "asset" NonAdaNativeToken
        :> "utxos"
        :> Pagination
        :> Get '[JSON] TimestampedAssetUTxOs
  -- ^ UTxOs that contain a specific asset.
  }
  deriving (Generic)
