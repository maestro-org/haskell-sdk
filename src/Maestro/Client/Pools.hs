module Maestro.Client.Pools
  (
    listPools
  , poolBlocks
  , poolDelegators
  , poolHistory
  , poolMetadata
  , poolRelays
  , poolInfo
  , poolUpdates
  ) where

import           Data.Text               (Text)
import           Maestro.API
import           Maestro.API.Pool
import           Maestro.Client
import           Maestro.Client.Env
import           Maestro.Types.Common
import           Maestro.Types.Pool
import           Maestro.Util.Pagination (Page)
import           Servant.API.Generic
import           Servant.Client

type PoolId = Text
poolsClient :: MaestroEnv -> PoolAPI (AsClientT IO)
poolsClient = fromServant . _pools  . apiClient

parseEpochNo :: Maybe EpochNo -> Maybe Integer
parseEpochNo = fmap (toInteger . unEpochNo)

-- | Returns a list of currently registered stake pools
listPools :: MaestroEnv -> Page -> IO [Pool]
listPools = _listPools . poolsClient

-- | Return information about blocks minted by a given pool for all epochs
-- (or just epoch `epoch_no` if provided)
poolBlocks :: MaestroEnv -> PoolId -> Page -> Maybe EpochNo -> Maybe Order -> IO [PoolBlock]
poolBlocks  = _poolBlocks . poolsClient

poolDelegators :: MaestroEnv -> PoolId -> Page -> IO [DelegatorInfo]
poolDelegators = _poolDelegators . poolsClient

-- | Returns per-epoch information about the specified pool
--  (or just epoch `epoch_no` if provided)
poolHistory :: MaestroEnv -> PoolId -> Page -> Maybe EpochNo -> Maybe Order -> IO [PoolHistory]
poolHistory = _poolHistory . poolsClient

-- | Returns current information about the specified pool
poolInfo :: MaestroEnv -> PoolId -> IO PoolInfo
poolInfo = _poolInfo . poolsClient

-- | Returns the metadata declared by a specific pool
poolMetadata :: MaestroEnv -> PoolId -> IO PoolMetadata
poolMetadata = _poolMetadata . poolsClient

-- | Returns a list of relays declared by the specified pool
poolRelays :: MaestroEnv -> PoolId -> IO [PoolRelay]
poolRelays = _poolRelays . poolsClient

-- | Returns a list of updates relating to the specified pool
poolUpdates :: MaestroEnv -> PoolId -> IO [PoolUpdates]
poolUpdates = _poolUpdates . poolsClient
