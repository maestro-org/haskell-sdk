-- | Module to define types for /\"General\"/ endpoints defined at [reference.gomaestro.org](https://reference.gomaestro.org/).

module Maestro.Types.General
  ( -- * Types for @/system-start@ endpoint
    SystemStart (..)
    -- * Types for @/era-history@ endpoint
  , SlotNo (..)
  , EpochNo (..)
  , EpochSize (..)
  , EraSummary (..)
  , EraParameters (..)
  , EraBound (..)
    -- * Types for @/protocol-params@ endpoint
  , ProtocolVersion (..)
  , MemoryStepsWith (..)
  , CostModel (..)
  , CostModels (..)
  , ProtocolParameters (..)
  ) where

import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           Data.Time            (LocalTime, NominalDiffTime)
import           Data.Word            (Word64)
import           Deriving.Aeson
import           Maestro.Types.Common (LowerFirst)
import           Numeric.Natural      (Natural)

------------------------------------------------------------------
--  Types for @/system-start@ endpoint.
------------------------------------------------------------------

-- | Network start time since genesis.
newtype SystemStart = SystemStart { _systemStartTime :: LocalTime }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_systemStart", CamelToSnake]] SystemStart

------------------------------------------------------------------
--  Types for @/era-history@ endpoint
------------------------------------------------------------------

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
  { _eraSummaryStart      :: !EraBound
  -- ^ Start of this era.
  , _eraSummaryEnd        :: !(Maybe EraBound)
  -- ^ End of this era.
  , _eraSummaryParameters :: !EraParameters
  -- ^ Parameters of this era.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_eraSummary", LowerFirst]] EraSummary

-- | Parameters for a network era which can vary between hardforks.
data EraParameters = EraParameters
  { _eraParametersEpochLength :: !EpochSize
  -- ^ Number of slots in an epoch.
  , _eraParametersSlotLength  :: !NominalDiffTime
  -- ^ How long a slot lasts.
  , _eraParametersSafeZone    :: !Word64
  -- ^ Number of slots from the tip of the ledger in which a hardfork will not happen.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_eraParameters", CamelToSnake]] EraParameters

-- | Bounds of an era.
data EraBound = EraBound
  { _eraBoundEpoch :: !EpochNo
  -- ^ Epoch number bounding this era.
  , _eraBoundSlot  :: !SlotNo
  -- ^ Absolute slot number bounding this era.
  , _eraBoundTime  :: !NominalDiffTime
  -- ^ Time relative to the start time of the network.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_eraBound", LowerFirst]] EraBound

------------------------------------------------------------------
--  Types for @/protocol-params@ endpoint.
------------------------------------------------------------------

-- | Current accepted protocol version. An increase in the major version indicates a hard fork, and the minor version a soft fork (meaning old software can validate but not produce new blocks).
data ProtocolVersion = ProtocolVersion
  { _protocolVersionMajor :: !Natural
  -- ^ Accepted protocol major version.
  , _protocolVersionMinor :: !Natural
  -- ^ Accepted protocol minor version.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_protocolVersion", LowerFirst]] ProtocolVersion

-- | Pair of memory & steps for the given type.
data MemoryStepsWith i = MemoryStepsWith
  { _memoryStepsWithMemory :: !i
  , _memoryStepsWithSteps  :: !i
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_memoryStepsWith", LowerFirst]] (MemoryStepsWith i)

-- | A cost model is a vector of coefficients that are used to compute the execution units required to execute a script. Its specifics depend on specific versions of the Plutus interpreter it is used with.
newtype CostModel = CostModel (Map Text Integer)
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Cost models (see `CostModel`) for script languages that use them.
data CostModels = CostModels
  { _costModelsPlutusV1 :: !CostModel
  , _costModelsPlutusV2 :: !CostModel
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_costModels", Rename "PlutusV1" "plutus:v1", Rename "PlutusV2" "plutus:v2"]] CostModels

-- | Protocol parameters for the latest epoch.
data ProtocolParameters = ProtocolParameters
  { _protocolParametersProtocolVersion                 :: !ProtocolVersion
  -- ^ See `ProtocolVersion`.
  , _protocolParametersMinFeeConstant                  :: !Natural
  -- ^ The linear factor for the minimum fee calculation for given epoch /AKA/ @min_fee_a@ and @tx_fee_fixed@.
  , _protocolParametersMinFeeCoefficient               :: !Natural
  -- ^ The constant factor for the minimum fee calculation /AKA/ @min_fee_b@ and @tx_fee_per_byte@.
  , _protocolParametersMaxBlockBodySize                :: !Natural
  -- ^ Maximum block body size.
  , _protocolParametersMaxBlockHeaderSize              :: !Natural
  -- ^ Maximum block header size.
  , _protocolParametersMaxTxSize                       :: !Natural
  -- ^ Maximum transaction size.
  , _protocolParametersStakeKeyDeposit                 :: !Natural
  -- 	The deposit required to register a stake address.
  , _protocolParametersPoolDeposit                     :: !Natural
  -- ^ The amount of a pool registration deposit in lovelaces /AKA/ @stake_pool_deposit@.
  , _protocolParametersPoolRetirementEpochBound        :: !EpochNo
  -- ^ The maximum number of epochs into the future that stake pools are permitted to schedule a retirement /AKA/ @pool_retire_max_epoch@, @e_max@.
  , _protocolParametersDesiredNumberOfPools            :: !Natural
  -- The equilibrium target number of stake pools. This is the \"k\" incentives parameter from the design document, /AKA/ @n_opt@, @stake_pool_target@.
  , _protocolParametersPoolInfluence                   :: !Rational
  -- The influence of the pledge in stake pool rewards. This is the \"a_0\" incentives parameter from the design document.
  , _protocolParametersMonetaryExpansion               :: !Rational
  -- ^ The monetary expansion rate. This determines the fraction of the reserves that are added to the fee pot each epoch. This is the \"rho\" incentives parameter from the design document.
  , _protocolParametersTreasuryExpansion               :: !Rational
  -- ^ The fraction of the fee pot each epoch that goes to the treasury. This is the \"tau\" incentives parameter from the design document, /AKA/ @treasury_cut@.
  , _protocolParametersMinPoolCost                     :: !Natural
  -- ^ The minimum value that stake pools are permitted to declare for their cost parameter.
  , _protocolParametersPrices                          :: !(MemoryStepsWith Rational)
  -- ^ The price per unit memory & price per reduction step corresponding to abstract notions of the relative memory usage and script execution steps respectively.
  , _protocolParametersMaxExecutionUnitsPerTransaction :: !(MemoryStepsWith Natural)
  -- ^ The maximum number of execution memory & steps allowed to be used in a single transaction.
  , _protocolParametersMaxExecutionUnitsPerBlock       :: !(MemoryStepsWith Natural)
  -- ^ The maximum number of execution memory & steps allowed to be used in a single block.
  , _protocolParametersMaxValueSize                    :: !Natural
  -- ^ Maximum size of the /value/ part of an output in a serialized transaction.
  , _protocolParametersCollateralPercentage            :: !Natural
  -- ^ The percentage of the transactions fee which must be provided as collateral when including non-native scripts.
  , _protocolParametersMaxCollateralInputs             :: !Natural
  -- ^ The maximum number of collateral inputs allowed in a transaction.
  , _protocolParametersCoinsPerUtxoByte                :: !Natural
  -- ^ The cost per UTxO size. Cost per UTxO /word/ for Alozno. Cost per UTxO /byte/ for Babbage and later.
  , _protocolParametersCostModels                      :: !CostModels
  -- ^ See `CostModels`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_protocolParameters", CamelToSnake]] ProtocolParameters

