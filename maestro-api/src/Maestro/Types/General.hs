module Maestro.Types.General
  ( -- * Types for @/system-start@ endpoint
    SystemStart (..)
    -- * Types for @/protocol-params@ endpoint
  , SlotNo (..)
  , EpochNo (..)
  , EpochSize (..)
  , EraSummary (..)
  , EraParameters (..)
  , EraBound (..)
  ) where

import           Data.Time            (LocalTime, NominalDiffTime)
import           Data.Word            (Word64)
import           Deriving.Aeson
import           Maestro.Types.Common (LowerFirst)

-- | Network start time since genesis.
newtype SystemStart = SystemStart { _systemStartTime :: LocalTime }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_systemStart", CamelToSnake]] SystemStart

-- | The 0-based index for the Ourboros time slot.
newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Enum, Bounded, Num, ToJSON, FromJSON)

-- | An epoch, i.e. the number of the epoch.
newtype EpochNo = EpochNo {unEpochNo :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral, ToJSON, FromJSON)

-- | Length of an epoch, i.e., number of slots in it.
newtype EpochSize = EpochSize {unEpochSize :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral, ToJSON, FromJSON)

-- | Network era summary.
data EraSummary = EraSummary
    { _eraSummaryStart      :: !EraBound  -- ^ Start of this era.
    , _eraSummaryEnd        :: !(Maybe EraBound)  -- ^ End of this era.
    , _eraSummaryParameters :: !EraParameters  -- ^ Parameters of this era.
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_eraSummary", LowerFirst]] EraSummary

-- | Parameters for a network era which can vary between hardforks.
data EraParameters = EraParameters
    { _eraParametersEpochLength :: !EpochSize  -- ^ Number of slots in an epoch.
    , _eraParametersSlotLength  :: !NominalDiffTime  -- ^ How long a slot lasts.
    , _eraParametersSafeZone    :: !Word64  -- ^ Number of slots from the tip of the ledger in which a hardfork will not happen.
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_eraParameters", CamelToSnake]] EraParameters

-- | Bounds of an era.
data EraBound = EraBound
    { _eraBoundEpoch :: !EpochNo  -- ^ Epoch number bounding this era.
    , _eraBoundSlot  :: !SlotNo  -- ^ Absolute slot number bounding this era.
    , _eraBoundTime  :: !NominalDiffTime  -- ^ Time relative to the start time of the network.
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_eraBound", LowerFirst]] EraBound
