module Maestro.Client.V0.Pools
  ( listPools,
    poolBlocks,
    poolDelegators,
    poolHistory,
    poolMetadata,
    poolRelays,
    poolInfo,
    poolUpdates,
  )
where

import           Maestro.API.V0
import           Maestro.API.V0.Pool
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

poolsClient :: MaestroEnv 'V0 -> PoolAPI (AsClientT IO)
poolsClient = fromServant . _pools . apiV0Client

-- | Returns a list of currently registered stake pools
listPools :: MaestroEnv 'V0 -> Page -> IO [PoolListInfo]
listPools = _listPools . poolsClient

-- | Return information about blocks minted by a given pool for all epochs
-- (or just epoch `epoch_no` if provided)
poolBlocks :: MaestroEnv 'V0 -> Bech32StringOf PoolId -> Page -> Maybe EpochNo -> Maybe Order -> IO [PoolBlock]
poolBlocks = _poolBlocks . poolsClient

poolDelegators :: MaestroEnv 'V0 -> Bech32StringOf PoolId -> Page -> IO [DelegatorInfo]
poolDelegators = _poolDelegators . poolsClient

-- | Returns per-epoch information about the specified pool
--  (or just epoch `epoch_no` if provided)
poolHistory :: MaestroEnv 'V0 -> Bech32StringOf PoolId -> Page -> Maybe EpochNo -> Maybe Order -> IO [PoolHistory]
poolHistory = _poolHistory . poolsClient

-- | Returns current information about the specified pool
poolInfo :: MaestroEnv 'V0 -> Bech32StringOf PoolId -> IO PoolInfo
poolInfo = _poolInfo . poolsClient

-- | Returns the metadata declared by a specific pool
poolMetadata :: MaestroEnv 'V0 -> Bech32StringOf PoolId -> IO PoolMetadata
poolMetadata = _poolMetadata . poolsClient

-- | Returns a list of relays declared by the specified pool
poolRelays :: MaestroEnv 'V0 -> Bech32StringOf PoolId -> IO [PoolRelay]
poolRelays = _poolRelays . poolsClient

-- | Returns a list of updates relating to the specified pool
poolUpdates :: MaestroEnv 'V0 -> Bech32StringOf PoolId -> IO [PoolUpdate]
poolUpdates = _poolUpdates . poolsClient
