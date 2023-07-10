-- | Module to define typeclass for timestamped response, i.e. response which has a @data@ & @last_updated@ field.

module Maestro.Types.V1.Common.Timestamped (
    LastUpdated (..),
    IsTimestamped (..),
  ) where

import           Data.Kind            (Type)
import           Deriving.Aeson
import           Maestro.Types.Common (BlockHash, SlotNo)

-- | Details of the most recent block processed by the indexer (aka chain tip); that is, the data returned is correct as of this block in time.
data LastUpdated = LastUpdated
  { _lastUpdatedBlockHash :: !BlockHash
  -- ^ Hash of the latest block.
  , _lastUpdatedBlockSlot :: !SlotNo
  -- ^ Slot number for the tip.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_lastUpdated", CamelToSnake]] LastUpdated

-- | Is the endpoint timestamped?
class IsTimestamped a where
  -- | What is the type of the main data in question?
  type TimestampedData a :: Type
  -- | Get the main data from the value of the given type @a@.
  getTimestampedData :: a -> TimestampedData a
  -- | Get the `LastUpdated` field.
  getTimestamp :: a -> LastUpdated
