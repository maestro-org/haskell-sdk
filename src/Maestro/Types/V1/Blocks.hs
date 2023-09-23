-- | Module to define types for /\"Blocks\"/ category endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/blocks).

module Maestro.Types.V1.Blocks
  ( BlockDetails (..)
  , TimestampedBlockDetails (..)
  ) where

import           Data.Time               (LocalTime)
import           Data.Word               (Word32, Word64)
import           Deriving.Aeson
import           Maestro.Types.Common
import           Maestro.Types.V1.Common

-- | Complete block details when queried by its hash or height.
data BlockDetails = BlockDetails
  { blockDetailsHash              :: !BlockHash
  -- ^ Block hash (identifier)
  , blockDetailsHeight            :: !BlockHeight
  -- ^ Block height
  , blockDetailsAbsoluteSlot      :: !AbsoluteSlot
  -- ^ Absolute slot of the block which includes the transaction
  , blockDetailsBlockProducer     :: !(Maybe (Bech32StringOf PoolId))
  -- ^ Block producer identifier name
  , blockDetailsConfirmations     :: !Word64
  -- ^ Total number of confirmations
  , blockDetailsEpoch             :: !EpochNo
    -- ^ Epoch number
  , blockDetailsEpochSlot         :: !SlotNo
    -- ^ Epoch Slot number
  , blockDetailsPreviousBlock     :: !(Maybe BlockHash)
    -- ^ Previous block hash
  , blockDetailsScriptInvocations :: !Word32
    -- ^ Previous block hash
  , blockDetailsSize              :: !Word32
    -- ^ Block total size
  , blockDetailsTimestamp         :: !LocalTime
    -- ^ UNIX timestamp
  , blockDetailsTotalFees         :: !Word64
    -- ^ Total number of fees
  , blockDetailsTxHashes          :: ![TxHash]
    -- ^ List of transaction hashes included on the block
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "blockDetails", CamelToSnake]] BlockDetails

-- | Timestamped `BlockDetails` response.
data TimestampedBlockDetails = TimestampedBlockDetails
  { timestampedBlockDetailsData        :: !BlockDetails
  -- ^ See `BlockDetails`.
  , timestampedBlockDetailsLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedBlockDetails", CamelToSnake]] TimestampedBlockDetails

instance IsTimestamped TimestampedBlockDetails where
  type TimestampedData TimestampedBlockDetails = BlockDetails
  getTimestampedData = timestampedBlockDetailsData
  getTimestamp = timestampedBlockDetailsLastUpdated
