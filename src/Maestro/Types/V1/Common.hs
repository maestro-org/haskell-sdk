module Maestro.Types.V1.Common
  ( LastUpdated (..),
    module Maestro.Types.Common
  )
where

import           Deriving.Aeson
import           Maestro.Types.Common

-- | Details of the most recent block processed by the indexer (aka chain tip); that is, the data returned is correct as of this block in time.
data LastUpdated = LastUpdated
  { _lastUpdatedBlockHash :: !BlockHash
  -- ^ Hash of the latest block.
  , _lastUpdatedBlockSlot :: !SlotNo
  -- ^ Slot number for the tip.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_lastUpdated", CamelToSnake]] LastUpdated
