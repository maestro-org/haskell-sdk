module Maestro.Client.V0.Assets where

import           Data.Text              (Text)
import           Maestro.API.V0         (_assets)
import           Maestro.API.V0.Assets
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

assetClient :: MaestroEnv 'V0 -> AssetsAPI (AsClientT IO)
assetClient = fromServant . _assets . apiV0Client

-- |
-- Returns list of  Information about the assets of the given policy ID
listAssetInfoByPolicyId ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | The Hex encoded policy ID
  PolicyId ->
  -- | Pagination
  Page ->
  IO [AssetInfo]
listAssetInfoByPolicyId = _assetPolicyInfo . assetClient

-- |
--  Returns a list of addresses which holding some of an asset of the given policy ID
listAssetAddressByPolicyId ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | The Hex encoded policy ID
  PolicyId ->
  -- | Pagination
  Page ->
  IO [Text]
listAssetAddressByPolicyId = _assetPolicyAddress . assetClient

-- |
-- Returns list of transactions in which an address receives an asset of the specified policy
listTxByPolicyId ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | The Hex encoded policy ID
  PolicyId ->
  -- | Pagination
  Page ->
  IO [AssetTx]
listTxByPolicyId = _assetPolicyTxs . assetClient

-- |
-- Returns UTxOs which contain assets of the given policy ID, with the asset names and amounts
listUtxosByPolicyId ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | The Hex encoded policy ID
  PolicyId ->
  -- | Pagination
  Page ->
  IO [PolicyUtxo]
listUtxosByPolicyId = _assetPolicyUtxos . assetClient

-- |
-- Returns UTxOs which contain assets of the given policy ID, with the asset names and amounts
getAssetDetail ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  IO AssetInfo
getAssetDetail = _assetDetail . assetClient

-- |
-- Returns a list of addresses which hold some amount of the specified asset
listAssetAddresses ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  Page ->
  IO [Text]
listAssetAddresses = _assetAddresses . assetClient

-- |
-- Returns list of transactions in which an address receives an asset of the specified policy
listAssetTx ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  -- | Return only transactions after supplied block height
  Maybe Integer ->
  -- | The Pagination
  Page ->
  Maybe Order ->
  IO [AssetTx]
listAssetTx = _assetTxs . assetClient

-- |
-- Returns list of transactions which minted or burned the specified asset
listAssetUpdates ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  -- | The Pagination
  Page ->
  -- | The order in which the results are sorted (by block height)
  Maybe Order ->
  IO [MintingTx]
listAssetUpdates = _assetUpdates . assetClient

-- |
-- Returns UTxOs containing the specified asset, each paired with the amount of the asset
listAssetUtxos ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  -- | The Pagination
  Page ->
  IO [AssetUtxo]
listAssetUtxos = _assetUtxos . assetClient
