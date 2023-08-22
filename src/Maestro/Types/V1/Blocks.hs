-- | Module to define types for /\"Blocks\"/ category endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/blocks).

module Maestro.Types.V1.Blocks
  ( BlockDetails (..)
  , TimestampedBlockDetails (..)
  ) where

import           Data.Time               (LocalTime)
import           Data.Word               (Word64, Word32)
import           Deriving.Aeson
import           Maestro.Types.Common
import           Maestro.Types.V0.Pool   (PoolId)
import           Maestro.Types.V1.Common

-- | Complete block Details when queried by its hash or height.
data BlockDetails = BlockDetails
  { _blockDetailsHash :: !BlockHash
  -- ^ Block hash (identifier)
  , _blockDetailsHeight :: !BlockHeight
  -- ^ Block height
  , _blockDetailsAbsoluteSlot :: !AbsoluteSlot
  -- ^ Absolute slot of the block which includes the transaction
  , _blockDetailsBlockProducer :: !(Maybe (Bech32StringOf PoolId))
  -- ^ Block producer identifier name
  , _blockDetailsConfirmations :: !Word64
  -- ^ Total number of confirmations
  , _blockDetailsEpoch :: !EpochNo
    -- ^ Epoch number
  , _blockDetailsEpochSlot :: !SlotNo
    -- ^ Epoch Slot number
  , _blockDetailsPreviousBlock :: !(Maybe BlockHash)
    -- ^ Previous block hash
  , _blockDetailsScriptInvocations :: !Word32
    -- ^ Previous block hash
  , _blockDetailsSize :: !Word32
    -- ^ Block total size
  , _blockDetailsTimestamp :: !LocalTime
    -- ^ UNIX timestamp
  , _blockDetailsTotalFees  :: !Word64
    -- ^ Total number of fees
  , _blockDetailsTxHashes :: ![TxHash]
    -- ^ List of transaction hashes included on the block
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_blockDetails", CamelToSnake]] BlockDetails

-- | Timestamped `BlockDetails` response.
data TimestampedBlockDetails = TimestampedBlockDetails
  { _timestampedBlockDetailsData        :: !BlockDetails
  -- ^ See `BlockDetails`.
  , _timestampedBlockDetailsLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_timestampedBlockDetails", CamelToSnake]] TimestampedBlockDetails

instance IsTimestamped TimestampedBlockDetails where
  type TimestampedData TimestampedBlockDetails = BlockDetails
  getTimestampedData = _timestampedBlockDetailsData
  getTimestamp = _timestampedBlockDetailsLastUpdated
