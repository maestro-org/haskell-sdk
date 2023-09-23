-- | Module to define types for /\"Pools\"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/pools).

module Maestro.Types.V1.Pools
  ( PoolListInfo (..),
    PaginatedPoolListInfo (..),
  )
where

import           Deriving.Aeson
import           Maestro.Types.V1.Common

-- | Information about a registered stake pool.
data PoolListInfo = PoolListInfo
  { poolListInfoPoolIdBech32 :: !(Bech32StringOf PoolId),
  -- ^ Bech32 encoded Pool ID.
    poolListInfoTicker       :: !(Maybe (TaggedText "pool-ticker"))
  -- ^ Pool ticker symbol.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "poolListInfo", CamelToSnake]] PoolListInfo

-- | Paginated list of registered stake pools.
data PaginatedPoolListInfo = PaginatedPoolListInfo
  { paginatedPoolListInfoData        :: ![PoolListInfo]
  -- ^ See `PoolListInfo`.
  , paginatedPoolListInfoLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  , paginatedPoolListInfoNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedPoolListInfo", CamelToSnake]] PaginatedPoolListInfo

instance IsTimestamped PaginatedPoolListInfo where
  type TimestampedData PaginatedPoolListInfo = [PoolListInfo]
  getTimestampedData = paginatedPoolListInfoData
  getTimestamp = paginatedPoolListInfoLastUpdated

instance HasCursor PaginatedPoolListInfo where
  getNextCursor = paginatedPoolListInfoNextCursor
