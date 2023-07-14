module Maestro.API.V0.Assets where

import           Data.Text                      (Text)
import           Maestro.Client.V0.Core.Pagination
import           Maestro.Types.V0
import           Servant.API
import           Servant.API.Generic

data AssetsAPI route = AssetsAPI
  { _assetPolicyInfo ::
      route
        :- "policy"
        :> Capture "policy" PolicyId
        :> Pagination
        :> Get '[JSON] [AssetInfo],
    _assetPolicyAddress ::
      route
        :- "policy"
        :> Capture "policy" PolicyId
        :> "addresses"
        :> Pagination
        :> Get '[JSON] [Text],
    _assetPolicyTxs ::
      route
        :- "policy"
        :> Capture "policy" PolicyId
        :> "txs"
        :> Pagination
        :> Get '[JSON] [AssetTx],
    _assetPolicyUtxos ::
      route
        :- "policy"
        :> Capture "policy" PolicyId
        :> "utxos"
        :> Pagination
        :> Get '[JSON] [PolicyUtxo],
    _assetDetail ::
      route
        :- Capture "asset" AssetId
        :> Get '[JSON] AssetInfo,
    _assetAddresses ::
      route
        :- Capture "asset" AssetId
        :> "addresses"
        :> Pagination
        :> Get '[JSON] [Text],
    _assetTxs ::
      route
        :- Capture "asset" AssetId
        :> "txs"
        :> QueryParam "from_height" Integer
        :> Pagination
        :> QueryParam "order" Order
        :> Get '[JSON] [AssetTx],
    _assetUpdates ::
      route
        :- Capture "asset" AssetId
        :> "updates"
        :> Pagination
        :> QueryParam "order" Order
        :> Get '[JSON] [MintingTx],
    _assetUtxos ::
      route
        :- Capture "asset" AssetId
        :> "utxos"
        :> Pagination
        :> Get '[JSON] [AssetUtxo]
  }
  deriving (Generic)
