module Maestro.API.V0.Pool where

import           Maestro.Client.V0.Core.Pagination
import           Maestro.Types.V0
import           Servant.API
import           Servant.API.Generic

data PoolAPI route = PoolAPI
  { _listPools ::
      route
        :- Pagination
        :> Get '[JSON] [PoolListInfo],
    _poolBlocks ::
      route
        :- Capture "pool_id" (Bech32StringOf PoolId)
        :> "blocks"
        :> Pagination
        :> QueryParam "epoch_no" EpochNo
        :> QueryParam "order" Order
        :> Get '[JSON] [PoolBlock],
    _poolDelegators ::
      route
        :- Capture "pool_id" (Bech32StringOf PoolId)
        :> "delegators"
        :> Pagination
        :> Get '[JSON] [DelegatorInfo],
    _poolHistory ::
      route
        :- Capture "pool_id" (Bech32StringOf PoolId)
        :> "history"
        :> Pagination
        :> QueryParam "epoch_no" EpochNo
        :> QueryParam "order" Order
        :> Get '[JSON] [PoolHistory],
    _poolInfo ::
      route
        :- Capture "pool_id" (Bech32StringOf PoolId)
        :> "info"
        :> Get '[JSON] PoolInfo,
    _poolMetadata ::
      route
        :- Capture "pool_id" (Bech32StringOf PoolId)
        :> "metadata"
        :> Get '[JSON] PoolMetadata,
    _poolRelays ::
      route
        :- Capture "pool_id" (Bech32StringOf PoolId)
        :> "relays"
        :> Get '[JSON] [PoolRelay],
    _poolUpdates ::
      route
        :- Capture "pool_id" (Bech32StringOf PoolId)
        :> "updates"
        :> Get '[JSON] [PoolUpdate]
  }
  deriving (Generic)
