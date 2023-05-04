module Maestro.Client.Assets where

import           Data.Text               (Text)
import           Maestro.API             (_assets)
import           Maestro.API.Assets
import           Maestro.Client
import           Maestro.Client.Env
import           Maestro.Types.Assets
import           Maestro.Types.Common
import           Maestro.Util.Pagination (Page)
import           Servant.API
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
             -> MaestroOrder
             -> IO [MaestroAssetTx]

listAssetTx mEnv asset bHeight pagination sortOrder =
  _listAssetTx mEnv asset bHeight pagination (Just $ show sortOrder)

  where
    _listAssetTx  :: MaestroEnv ->  Asset -> Maybe Integer -> Page -> Maybe String -> IO [MaestroAssetTx]
    _listAssetTx = _assetTxs . assetClient

-- |
-- Returns list of transactions which minted or burned the specified asset
--
listAssetUpdates  :: MaestroEnv       -- ^ The Maestro Environment
                  ->  Asset                -- ^ Asset, encoded as concatenation of hex of policy ID and asset name
                  -> Page                  -- ^ The Pagination
                  -> MaestroOrder          -- ^ The order in which the results are sorted (by block height)
                  -> IO [MaestroAssetUpdates]

listAssetUpdates mEnv asset pagination sortOrder =
    _listSortedAssetUpdates mEnv asset pagination (Just $ show sortOrder)
    where
      _listSortedAssetUpdates = _assetUpdates . assetClient

-- |
-- Returns UTxOs containing the specified asset, each paired with the amount of the asset
--
listAssetUtxos  :: MaestroEnv         -- ^ The Maestro Environment
                ->  Asset             -- ^ Asset, encoded as concatenation of hex of policy ID and asset name
                -> Page               -- ^ The Pagination
                -> IO [MaestroAssetUtxo]

listAssetUtxos = _assetUtxos . assetClient
