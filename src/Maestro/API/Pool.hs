module Maestro.API.Pool where

import           Data.Text               (Text)
import           Maestro.Types
import           Maestro.Util.Pagination
import           Servant.API
import           Servant.API.Generic

data PoolAPI route = PoolAPI
  {
    _listPools
      :: route
      :- Pagination
      :> Get '[JSON]  [Pool]

  , _poolBlocks
    :: route
    :- Capture  "pool_id" Text
    :> "blocks"
    :> Pagination
    :> QueryParam "epoch_no" EpochNo
    :> QueryParam "order" Order
    :> Get '[JSON]  [PoolBlock]

  , _poolDelegators
    :: route
    :- Capture  "pool_id" Text
    :> "delegators"
    :> Pagination
    :> Get '[JSON]  [DelegatorInfo]

  , _poolHistory
    :: route
    :- Capture  "pool_id" Text
    :> "history"
    :> Pagination
    :> QueryParam "epoch_no" EpochNo
    :> QueryParam "order" Order
    :> Get '[JSON]  [PoolHistory]

  , _poolInfo
    :: route
    :- Capture  "pool_id" Text
    :> "info"
    :> Get '[JSON] PoolInfo

  , _poolMetadata
    :: route
    :- Capture  "pool_id" Text
    :> "metadata"
    :> Get '[JSON] PoolMetadata

  , _poolRelays
    :: route
    :- Capture  "pool_id" Text
    :> "relays"
    :> Get '[JSON] [PoolRelay]

  , _poolUpdates
    :: route
    :- Capture  "pool_id" Text
    :> "updates"
    :> Get '[JSON] [PoolUpdates]
  } deriving (Generic)
