module Maestro.API.V1.Pools where

import           Maestro.Client.V1.Core.Pagination
import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

newtype PoolsAPI route = PoolsAPI
  { _listPools ::
      route
        :- Pagination
        :> Get '[JSON] PaginatedPoolListInfo
  }
  deriving (Generic)
