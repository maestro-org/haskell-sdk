module Maestro.Client.Assets where

import           Data.Text               (Text)
import           Maestro.API             (_assets)
import           Maestro.API.Assets
import           Maestro.Client
import           Maestro.Client.Env
import           Maestro.Types.Assets
import           Maestro.Types.Common
import           Maestro.Util.Pagination (Page)
import           Servant.API.Generic
import           Servant.Client

type PolicyId = Text
type Asset = Text


assetClient :: MaestroEnv -> AssetsAPI (AsClientT IO)
assetClient = fromServant . _assets . apiClient

-- |
-- Returns list of  Information about the assets of the given policy ID
--
listAssetInfoByPolicyId :: MaestroEnv           -- ^ The Maestro Environment
                        ->  PolicyId            -- ^ The Hex encoded policy ID
                        -> Page                 -- ^ Pagination
                        -> IO [MaestroAssetInfo]
listAssetInfoByPolicyId = _assetPolicyInfo . assetClient

-- |
--  Returns a list of addresses which holding some of an asset of the given policy ID
--
listAssetAddressByPolicyId  :: MaestroEnv   -- ^ The Maestro Environment
                            ->  PolicyId    -- ^ The Hex encoded policy ID
                            -> Page         -- ^ Pagination
                            -> IO [String]
listAssetAddressByPolicyId = _assetPolicyAddress . assetClient

-- |
-- Returns list of transactions in which an address receives an asset of the specified policy
--
listTxByPolicyId  :: MaestroEnv           -- ^ The Maestro Environment
                  ->  PolicyId            -- ^ The Hex encoded policy ID
                  -> Page                 -- ^ Pagination
                  -> IO [MaestroAssetTx]
listTxByPolicyId = _assetPolicyTxs . assetClient


-- |
-- Returns UTxOs which contain assets of the given policy ID, with the asset names and amounts
--
listUtxosByPolicyId  :: MaestroEnv            -- ^ The Maestro Environment
                     ->  PolicyId             -- ^ The Hex encoded policy ID
                     -> Page                  -- ^ Pagination
                     -> IO [MaestroAssetUtxo]
listUtxosByPolicyId = _assetPolicyUtxos . assetClient

-- |
-- Returns UTxOs which contain assets of the given policy ID, with the asset names and amounts
--
getAssetDetail  :: MaestroEnv         -- ^ The Maestro Environment
                ->  Asset             -- ^ Asset, encoded as concatenation of hex of policy ID and asset name
                -> IO MaestroAssetInfo

getAssetDetail = _assetDetail . assetClient

-- |
-- Returns a list of addresses which hold some amount of the specified asset
--
listAssetAddresses  :: MaestroEnv         -- ^ The Maestro Environment
                ->  Asset             -- ^ Asset, encoded as concatenation of hex of policy ID and asset name
                -> Page
                -> IO [String]

listAssetAddresses = _assetAddresses . assetClient


-- |
-- Returns list of transactions in which an address receives an asset of the specified policy
--
listAssetTx  :: MaestroEnv         -- ^ The Maestro Environment
             ->  Asset             -- ^ Asset, encoded as concatenation of hex of policy ID and asset name
             -> Maybe Integer      -- ^ Return only transactions after supplied block height
             -> Page               -- ^ The Pagination
             -> Maybe Order
             -> IO [MaestroAssetTx]

listAssetTx  = _assetTxs . assetClient

-- |
-- Returns list of transactions which minted or burned the specified asset
--
listAssetUpdates  :: MaestroEnv       -- ^ The Maestro Environment
                  ->  Asset                -- ^ Asset, encoded as concatenation of hex of policy ID and asset name
                  -> Page                  -- ^ The Pagination
                  -> Maybe Order          -- ^ The order in which the results are sorted (by block height)
                  -> IO [MaestroAssetUpdates]

listAssetUpdates = _assetUpdates . assetClient

-- |
-- Returns UTxOs containing the specified asset, each paired with the amount of the asset
--
listAssetUtxos  :: MaestroEnv         -- ^ The Maestro Environment
                ->  Asset             -- ^ Asset, encoded as concatenation of hex of policy ID and asset name
                -> Page               -- ^ The Pagination
                -> IO [MaestroAssetUtxo]

listAssetUtxos = _assetUtxos . assetClient
