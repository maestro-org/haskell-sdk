module Maestro.API.Assets where

import           Data.Text               (Text)
import           Maestro.Types.Assets
import           Maestro.Types.Common
import           Maestro.Util.Pagination
import           Servant.API
import           Servant.API.Generic

data AssetsAPI route = AssetsAPI
  {
    _assetPolicyInfo
      :: route
      :- "policy"
      :> Capture "policy" Text
      :> Pagination
      :> Get  '[JSON] [MaestroAssetInfo]

  , _assetPolicyAddress
      :: route
      :- "policy"
      :> Capture "policy" Text
      :> "addresses"
      :> Pagination
      :> Get  '[JSON] [String]

  , _assetPolicyTxs
      :: route
      :- "policy"
      :> Capture "policy" Text
      :> "txs"
      :> Pagination
      :> Get  '[JSON] [MaestroAssetTx]

  , _assetPolicyUtxos
      :: route
      :- "policy"
      :> Capture "policy" Text
      :> "utxos"
      :> Pagination
      :> Get  '[JSON] [MaestroAssetUtxo]

  , _assetDetail
      :: route
      :- Capture "asset" Text
      :> Get  '[JSON] MaestroAssetInfo

  , _assetAddresses
      :: route
      :- Capture "asset" Text
      :> "addresses"
      :> Pagination
      :> Get  '[JSON] [String]

  , _assetTxs
      :: route
      :- Capture "asset" Text
      :> "txs"
      :> QueryParam "from_height" Integer
      :> Pagination
      :> QueryParam "order" Order
      :> Get  '[JSON] [MaestroAssetTx]

  , _assetUpdates
      :: route
      :- Capture "asset" Text
      :> "updates"
      :> Pagination
      :> QueryParam "order" Order
      :> Get  '[JSON] [MaestroAssetUpdates]

  , _assetUtxos
      :: route
      :- Capture "asset" Text
      :> "utxos"
      :> Pagination
      :> Get  '[JSON] [MaestroAssetUtxo]
  } deriving (Generic)
