-- | Module to define types for /\"Pools\"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/pools).

module Maestro.Types.V1.Pools
  ( PoolId,
    PoolListInfo (..),
    PaginatedPoolListInfo (..),
  )
where

import           Deriving.Aeson
import           Maestro.Types.V1.Common

-- | Phantom datatype to be used with, say `Bech32StringOf` to represent Bech32 representation of a pool id.
data PoolId

-- | Information about a registered stake pool.
data PoolListInfo = PoolListInfo
  { _poolListInfoPoolIdBech32 :: !(Bech32StringOf PoolId),
  -- ^ Bech32 encoded Pool ID.
    _poolListInfoTicker       :: !(Maybe (TaggedText "pool-ticker"))
  -- ^ Pool ticker symbol.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolListInfo", CamelToSnake]] PoolListInfo

-- | Paginated list of registered stake pools.
data PaginatedPoolListInfo = PaginatedPoolListInfo
  { _paginatedPoolListInfoData        :: ![PoolListInfo]
  -- ^ See `PoolListInfo`.
  , _paginatedPoolListInfoLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  , _paginatedPoolListInfoNextCursor  :: !(Maybe NextCursor)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_paginatedPoolListInfo", CamelToSnake]] PaginatedPoolListInfo

instance IsTimestamped PaginatedPoolListInfo where
  type TimestampedData PaginatedPoolListInfo = [PoolListInfo]
  getTimestampedData = _paginatedPoolListInfoData
  getTimestamp = _paginatedPoolListInfoLastUpdated

instance HasCursor PaginatedPoolListInfo where
  getNextCursor = _paginatedPoolListInfoNextCursor
