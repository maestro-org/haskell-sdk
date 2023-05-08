module Maestro.Client.Assets where

import Maestro.API (_assets)
import Maestro.API.Assets
import Maestro.Client
import Maestro.Client.Env
import Maestro.Types.Assets
import Maestro.Types.Common
import Maestro.Util.Pagination (Page)
import Servant.API.Generic
import Servant.Client

assetClient :: MaestroEnv -> AssetsAPI (AsClientT IO)
assetClient = fromServant . _assets . apiClient

-- |
-- Returns list of  Information about the assets of the given policy ID
listAssetInfoByPolicyId ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | The Hex encoded policy ID
  PolicyId ->
  -- | Pagination
  Page ->
  IO [MaestroAssetInfo]
listAssetInfoByPolicyId = _assetPolicyInfo . assetClient

-- |
--  Returns a list of addresses which holding some of an asset of the given policy ID
listAssetAddressByPolicyId ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | The Hex encoded policy ID
  PolicyId ->
  -- | Pagination
  Page ->
  IO [String]
listAssetAddressByPolicyId = _assetPolicyAddress . assetClient

-- |
-- Returns list of transactions in which an address receives an asset of the specified policy
listTxByPolicyId ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | The Hex encoded policy ID
  PolicyId ->
  -- | Pagination
  Page ->
  IO [MaestroAssetTx]
listTxByPolicyId = _assetPolicyTxs . assetClient

-- |
-- Returns UTxOs which contain assets of the given policy ID, with the asset names and amounts
listUtxosByPolicyId ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | The Hex encoded policy ID
  PolicyId ->
  -- | Pagination
  Page ->
  IO [MaestroAssetUtxo]
listUtxosByPolicyId = _assetPolicyUtxos . assetClient

-- |
-- Returns UTxOs which contain assets of the given policy ID, with the asset names and amounts
getAssetDetail ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  IO MaestroAssetInfo
getAssetDetail = _assetDetail . assetClient

-- |
-- Returns a list of addresses which hold some amount of the specified asset
listAssetAddresses ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  Page ->
  IO [String]
listAssetAddresses = _assetAddresses . assetClient

-- |
-- Returns list of transactions in which an address receives an asset of the specified policy
listAssetTx ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  -- | Return only transactions after supplied block height
  Maybe Integer ->
  -- | The Pagination
  Page ->
  Maybe Order ->
  IO [MaestroAssetTx]
listAssetTx = _assetTxs . assetClient

-- |
-- Returns list of transactions which minted or burned the specified asset
listAssetUpdates ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  -- | The Pagination
  Page ->
  -- | The order in which the results are sorted (by block height)
  Maybe Order ->
  IO [MaestroAssetUpdates]
listAssetUpdates = _assetUpdates . assetClient

-- |
-- Returns UTxOs containing the specified asset, each paired with the amount of the asset
listAssetUtxos ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Asset, encoded as concatenation of hex of policy ID and asset name
  AssetId ->
  -- | The Pagination
  Page ->
  IO [MaestroAssetUtxo]
listAssetUtxos = _assetUtxos . assetClient
