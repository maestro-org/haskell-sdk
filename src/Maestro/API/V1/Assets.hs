module Maestro.API.V1.Assets where

import Maestro.Types.V1
import Servant.API
import Servant.API.Generic

data AssetsAPI route = AssetsAPI
  { assetInfo ::
      route
        :- Capture "asset" NonAdaNativeToken
          :> Get '[JSON] TimestampedAssetInfo
  -- ^ Native asset information.
  }
  deriving (Generic)
