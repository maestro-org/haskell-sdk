-- | Module to query for /"pools"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/pools).

module Maestro.Client.V1.Pools
  ( listPools,
  )
where

import           Maestro.API.V1
import qualified Maestro.API.V1.Pools   as Mapi
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.V1
import           Servant.API.Generic
import           Servant.Client

poolsClient :: MaestroEnv 'V1 -> Mapi.PoolsAPI (AsClientT IO)
poolsClient = fromServant . pools . apiV1Client

-- | Returns a list of currently registered stake pools.
listPools :: MaestroEnv 'V1 -> Cursor -> IO PaginatedPoolListInfo
listPools = Mapi.listPools . poolsClient
