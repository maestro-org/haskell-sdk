module Maestro.API.Assets where

import Maestro.Types.Assets
import Maestro.Types.Common
import Maestro.Util.Pagination
import Servant.API
import Servant.API.Generic

data AssetsAPI route = AssetsAPI
  { _assetPolicyInfo ::
      route
        :- "policy"
        :> Capture "policy" PolicyId
        :> Pagination
        :> Get '[JSON] [MaestroAssetInfo],
    _assetPolicyAddress ::
      route
        :- "policy"
        :> Capture "policy" PolicyId
        :> "addresses"
        :> Pagination
        :> Get '[JSON] [String],
    _assetPolicyTxs ::
      route
        :- "policy"
        :> Capture "policy" PolicyId
        :> "txs"
        :> Pagination
        :> Get '[JSON] [MaestroAssetTx],
    _assetPolicyUtxos ::
      route
        :- "policy"
        :> Capture "policy" PolicyId
        :> "utxos"
        :> Pagination
        :> Get '[JSON] [MaestroAssetUtxo],
    _assetDetail ::
      route
        :- Capture "asset" AssetId
        :> Get '[JSON] MaestroAssetInfo,
    _assetAddresses ::
      route
        :- Capture "asset" AssetId
        :> "addresses"
        :> Pagination
        :> Get '[JSON] [String],
    _assetTxs ::
      route
        :- Capture "asset" AssetId
        :> "txs"
        :> QueryParam "from_height" Integer
        :> Pagination
        :> QueryParam "order" Order
        :> Get '[JSON] [MaestroAssetTx],
    _assetUpdates ::
      route
        :- Capture "asset" AssetId
        :> "updates"
        :> Pagination
        :> QueryParam "order" Order
        :> Get '[JSON] [MaestroAssetUpdates],
    _assetUtxos ::
      route
        :- Capture "asset" AssetId
        :> "utxos"
        :> Pagination
        :> Get '[JSON] [MaestroAssetUtxo]
  }
  deriving (Generic)
