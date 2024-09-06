-- | Module to define types for /\"General\"/ endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/general).

module Maestro.Types.V1.General
  ( -- * Types for @/system-start@ endpoint
    TimestampedSystemStart (..)
    -- * Types for @/era-summaries@ endpoint
  , TimestampedEraSummaries (..)
  , EraSummary (..)
  , EraParameters (..)
  , EraBound (..)
  , EraBoundTime (..)
  , EpochSlotLength (..)
    -- * Types for @/protocol-parameters@ endpoint
  , AsAda (..)
  , AsLovelace (..)
  , AsBytes (..)
  , ProtocolVersion (..)
  , MemoryCpuWith (..)
  , CostModel (..)
  , CostModels (..)
  , MaestroRational (..)
  , textToMaestroRational
  , textFromMaestroRational
  , ConstitutionalCommittee (..)
  , ProtocolParametersUpdateDRep (..)
  , DRepVotingThresholds (..)
  , ProtocolParametersUpdateStakePool (..)
  , StakePoolVotingThresholds (..)
  , MinFeeReferenceScripts (..)
  , TimestampedProtocolParameters (..)
  , ProtocolParameters (..)
    -- * Types for @/chain-tip@ endpoint
  , TimestampedChainTip (..)
  , ChainTip (..)
  ) where

import           Control.Monad           (unless, when)
import           Data.Aeson              (FromJSON (parseJSON), toEncoding,
                                          toJSON, withText)
import           Data.Int                (Int64)
import           Data.Ratio              (denominator, numerator, (%))
import           Data.Text               (Text)
import qualified Data.Text               as Txt
import qualified Data.Text.Read          as TxtRead
import           Data.Time               (LocalTime, NominalDiffTime)
import           Data.Word               (Word64)
import           Deriving.Aeson
import           Maestro.Types.Common    (BlockHash, EpochNo, EpochSize,
                                          LowerFirst, SlotNo)
import           Maestro.Types.V1.Common (IsTimestamped (..), LastUpdated (..))
import           Numeric.Natural         (Natural)

------------------------------------------------------------------
--  Types for @/system-start@ endpoint.
------------------------------------------------------------------

-- | Network start time since genesis.
data TimestampedSystemStart = TimestampedSystemStart
  { timestampedSystemStartData        :: !LocalTime
  -- ^ Network start time since genesis.
  , timestampedSystemStartLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedSystemStart", CamelToSnake]] TimestampedSystemStart

instance IsTimestamped TimestampedSystemStart where
  type TimestampedData TimestampedSystemStart = LocalTime
  getTimestampedData = timestampedSystemStartData
  getTimestamp = timestampedSystemStartLastUpdated

------------------------------------------------------------------
--  Types for @/era-summaries@ endpoint
------------------------------------------------------------------

-- | Network era summaries.
data TimestampedEraSummaries = TimestampedEraSummaries
  { timestampedEraSummariesData        :: ![EraSummary]
  -- ^ Era summaries, see `EraSummary`.
  , timestampedEraSummariesLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedEraSummaries", CamelToSnake]] TimestampedEraSummaries

instance IsTimestamped TimestampedEraSummaries where
  type TimestampedData TimestampedEraSummaries = [EraSummary]
  getTimestampedData = timestampedEraSummariesData
  getTimestamp = timestampedEraSummariesLastUpdated

-- | Network era summary.
data EraSummary = EraSummary
  { eraSummaryStart      :: !EraBound
  -- ^ Start of this era.
  , eraSummaryEnd        :: !(Maybe EraBound)
  -- ^ End of this era.
  , eraSummaryParameters :: !EraParameters
  -- ^ Parameters of this era.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "eraSummary", LowerFirst]] EraSummary

newtype EpochSlotLength = EpochSlotLength
  { epochSlotLengthMilliseconds :: NominalDiffTime }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "epochSlotLength", LowerFirst]] EpochSlotLength

-- | Parameters for a network era which can vary between hardforks.
data EraParameters = EraParameters
  { eraParametersEpochLength :: !EpochSize
  -- ^ Number of slots in an epoch.
  , eraParametersSlotLength  :: !EpochSlotLength
  -- ^ How long a slot lasts.
  , eraParametersSafeZone    :: !(Maybe Word64)
  -- ^ Number of slots from the tip of the ledger in which a hardfork will not happen.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "eraParameters", CamelToSnake]] EraParameters

newtype EraBoundTime = EraBoundTime
  { eraBoundTimeSeconds :: NominalDiffTime
    -- ^ Time relative to the start time of the network.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "eraBoundTime", LowerFirst]] EraBoundTime

-- | Bounds of an era.
data EraBound = EraBound
  { eraBoundEpoch :: !EpochNo
  -- ^ Epoch number bounding this era.
  , eraBoundSlot  :: !SlotNo
  -- ^ Absolute slot number bounding this era.
  , eraBoundTime  :: !EraBoundTime
  -- ^ Time relative to the start time of the network.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "eraBound", LowerFirst]] EraBound

------------------------------------------------------------------
--  Types for @/protocol-parameters@ endpoint.
------------------------------------------------------------------

-- | Lovelaces.
newtype AsLovelace = AsLovelace
  { asLovelaceLovelace :: Natural
    -- ^ Lovelaces.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "asLovelace", LowerFirst]] AsLovelace

-- | Number of bytes.
newtype AsBytes = AsBytes
  { asBytesBytes :: Natural
    -- ^ Number of bytes.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "asBytes", LowerFirst]] AsBytes

-- | Represents lovelaces.
newtype AsAda = AsAda
  { asAdaAda :: AsLovelace
    -- ^ See `AsLovelace`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "asAda", LowerFirst]] AsAda

-- | Current accepted protocol version. An increase in the major version indicates a hard fork, and the minor version a soft fork (meaning old software can validate but not produce new blocks).
data ProtocolVersion = ProtocolVersion
  { protocolVersionMajor :: !Natural
  -- ^ Accepted protocol major version.
  , protocolVersionMinor :: !Natural
  -- ^ Accepted protocol minor version.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "protocolVersion", LowerFirst]] ProtocolVersion

-- | Pair of memory & cpu units for the given type.
data MemoryCpuWith i = MemoryCpuWith
  { memoryCpuWithMemory :: !i
  , memoryCpuWithCpu    :: !i
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "memoryCpuWith", LowerFirst]] (MemoryCpuWith i)

-- | A cost model is a vector of coefficients that are used to compute the execution units required to execute a script. Its specifics depend on specific versions of the Plutus interpreter it is used with.
newtype CostModel = CostModel [Int64]
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Cost models (see `CostModel`) for script languages that use them.
data CostModels = CostModels
  { costModelsPlutusV1 :: !CostModel
  , costModelsPlutusV2 :: !CostModel
  , costModelsPlutusV3 :: !CostModel
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "costModels", CamelToSnake]] CostModels

-- | Maestro's represents rational numbers as string with numerator and denominator demarcated by \'\/\', example: @"1/3"@.
newtype MaestroRational = MaestroRational { unMaestroRational :: Rational }
  deriving stock Eq

instance Show MaestroRational where
  show (MaestroRational r) = show (numerator r) ++ '/' : show (denominator r)

-- | Get original `Text` from `MaestroRational`.
textFromMaestroRational :: MaestroRational -> Text
textFromMaestroRational = Txt.pack . show . unMaestroRational

-- | Parses given `Text` to `MaestroRational`.
textToMaestroRational :: Text -> Either String MaestroRational
textToMaestroRational ratTxt =
  case TxtRead.signed rationalReader ratTxt of
    Right (rat, remainingTxt) -> if Txt.null remainingTxt
      then pure $ MaestroRational rat
      else Left "Expected full string to be parsed"
    Left e -> Left e
  where
    rationalReader :: TxtRead.Reader Rational
    rationalReader ratTxt' = do
        (numr, remaining) <- TxtRead.decimal ratTxt'
        (nextChar, denmrTxt) <- maybe
            (Left "Unexpected end of string after parsing numerator")
            pure
            $ Txt.uncons remaining
        unless (nextChar == '/')
            . Left
            $ "Expected numerator to be immediately followed by '/', but it was followed by: " ++ show nextChar
        (denmr, finalRemaining) <- TxtRead.decimal denmrTxt
        when (denmr == 0)
            $ Left "Expected non-zero denominator"
        pure (numr % denmr, finalRemaining)

instance ToJSON MaestroRational where
  toEncoding = toEncoding . textFromMaestroRational
  toJSON = toJSON . textFromMaestroRational

instance FromJSON MaestroRational where
  parseJSON = withText "MaestroRational" $ \ratTxt -> either fail pure $ textToMaestroRational ratTxt

data ConstitutionalCommittee = ConstitutionalCommittee
  { constitutionalCommitteeDefault             :: !MaestroRational
  , constitutionalCommitteeStateOfNoConfidence :: !MaestroRational
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "constitutionalCommittee", CamelToSnake]] ConstitutionalCommittee

data ProtocolParametersUpdateDRep = ProtocolParametersUpdateDRep
  { ppUpdateDrepEconomic   :: !MaestroRational
  , ppUpdateDrepGovernance :: !MaestroRational
  , ppUpdateDrepNetwork    :: !MaestroRational
  , ppUpdateDrepTechnical  :: !MaestroRational
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ppUpdateDrep", CamelToSnake]] ProtocolParametersUpdateDRep

-- | DRep voting thresholds.
data DRepVotingThresholds = DRepVotingThresholds
  { drepVotingThresholdsConstitution :: !MaestroRational
  , drepVotingThresholdsConstitutionalCommittee :: !ConstitutionalCommittee
  , drepVotingThresholdsHardForkInitiation :: !MaestroRational
  , drepVotingThresholdsNoConfidence :: !MaestroRational
  , drepVotingThresholdsProtocolParametersUpdate :: !ProtocolParametersUpdateDRep
  , drepVotingThresholdsTreasuryWithdrawals :: !MaestroRational
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "drepVotingThresholds", CamelToSnake]] DRepVotingThresholds

newtype ProtocolParametersUpdateStakePool = ProtocolParametersUpdateStakePool
  { ppUpdateStakePoolSecurity   :: MaestroRational
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ppUpdateStakePool", CamelToSnake]] ProtocolParametersUpdateStakePool

-- | Stake pool voting thresholds.
data StakePoolVotingThresholds = StakePoolVotingThresholds
  { stakePoolVotingThresholdsConstitutionalCommittee :: !ConstitutionalCommittee
  , stakePoolVotingThresholdsHardForkInitiation :: !MaestroRational
  , stakePoolVotingThresholdsNoConfidence :: !MaestroRational
  , stakePoolVotingThresholdsProtocolParametersUpdate :: !ProtocolParametersUpdateStakePool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "stakePoolVotingThresholds", CamelToSnake]] StakePoolVotingThresholds

data MinFeeReferenceScripts = MinFeeReferenceScripts
  { minFeeReferenceScriptsBase       :: !Natural
  , minFeeReferenceScriptsMultiplier :: !Rational
  , minFeeReferenceScriptsRange      :: !Natural
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "minFeeReferenceScripts", CamelToSnake]] MinFeeReferenceScripts

-- | Timestamped `ProtocolParameters` response.
data TimestampedProtocolParameters = TimestampedProtocolParameters
  { timestampedProtocolParametersData        :: !ProtocolParameters
  -- ^ See `ProtocolParametersData`.
  , timestampedProtocolParametersLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedProtocolParameters", CamelToSnake]] TimestampedProtocolParameters

instance IsTimestamped TimestampedProtocolParameters where
  type TimestampedData TimestampedProtocolParameters = ProtocolParameters
  getTimestampedData = timestampedProtocolParametersData
  getTimestamp = timestampedProtocolParametersLastUpdated

-- | Protocol parameters for the latest epoch.
data ProtocolParameters = ProtocolParameters
  {
    protocolParametersCollateralPercentage            :: !Natural
  -- ^ The percentage of the transactions fee which must be provided as collateral when including non-native scripts.
  , protocolParametersConstitutionalCommitteeMaxTermLength :: !Natural
  , protocolParametersConstitutionalCommitteeMinSize :: !Natural
  , protocolParametersDelegateRepresentativeDeposit :: !AsAda
  , protocolParametersDelegateRepresentativeMaxIdleTime :: !Natural
  , protocolParametersDelegateRepresentativeVotingThresholds :: !DRepVotingThresholds
  , protocolParametersDesiredNumberOfStakePools       :: !Natural
  -- The equilibrium target number of stake pools. This is the \"k\" incentives parameter from the design document, /AKA/ @n_opt@, @stake_pool_target@.
  , protocolParametersGovernanceActionDeposit :: !AsAda
  , protocolParametersGovernanceActionLifetime :: !Natural
  , protocolParametersMaxBlockBodySize                :: !AsBytes
  -- ^ Maximum block body size.
  , protocolParametersMaxBlockHeaderSize              :: !AsBytes
  -- ^ Maximum block header size.
  , protocolParametersMaxCollateralInputs             :: !Natural
  -- ^ The maximum number of collateral inputs allowed in a transaction.
  , protocolParametersMaxExecutionUnitsPerBlock       :: !(MemoryCpuWith Natural)
  -- ^ The maximum number of execution memory & steps allowed to be used in a single block.
  , protocolParametersMaxExecutionUnitsPerTransaction :: !(MemoryCpuWith Natural)
  -- ^ The maximum number of execution memory & steps allowed to be used in a single transaction.
  , protocolParametersMaxReferenceScriptsSize :: !AsBytes
  , protocolParametersMaxTransactionSize              :: !AsBytes
  -- ^ Maximum transaction size.
  , protocolParametersMaxValueSize                    :: !AsBytes
  -- ^ Maximum size of the /value/ part of an output in a serialized transaction.
  , protocolParametersMinFeeCoefficient               :: !Natural
  -- ^ The constant factor for the minimum fee calculation /AKA/ @min_fee_a@ and @tx_fee_per_byte@.
  , protocolParametersMinFeeConstant                  :: !AsAda
  -- ^ The linear factor for the minimum fee calculation for given epoch /AKA/ @min_fee_b@ and @tx_fee_fixed@.
  , protocolParametersMinFeeReferenceScripts :: !MinFeeReferenceScripts
  , protocolParametersMinStakePoolCost                :: !AsAda
  -- ^ The minimum value that stake pools are permitted to declare for their cost parameter.
  , protocolParametersMinUtxoDepositCoefficient       :: !Natural
  -- ^ The cost per UTxO size. Cost per UTxO /word/ for Alozno. Cost per UTxO /byte/ for Babbage and later.
  , protocolParametersMonetaryExpansion               :: !MaestroRational
  -- ^ The monetary expansion rate. This determines the fraction of the reserves that are added to the fee pot each epoch. This is the \"rho\" incentives parameter from the design document.
  , protocolParametersPlutusCostModels                :: !CostModels
  -- ^ See `CostModels`.
  , protocolParametersScriptExecutionPrices           :: !(MemoryCpuWith MaestroRational)
  -- ^ The price per unit memory & price per reduction step corresponding to abstract notions of the relative memory usage and script execution steps respectively.
  , protocolParametersStakeCredentialDeposit          :: !AsAda
  -- 	The deposit required to register a stake address.
  , protocolParametersStakePoolDeposit                :: !AsAda
  -- ^ The amount of a pool registration deposit in lovelaces /AKA/ @stake_pool_deposit@.
  , protocolParametersStakePoolPledgeInfluence        :: !MaestroRational
  -- The influence of the pledge in stake pool rewards. This is the \"a_0\" incentives parameter from the design document.
  , protocolParametersStakePoolRetirementEpochBound   :: !EpochNo
  -- ^ The maximum number of epochs into the future that stake pools are permitted to schedule a retirement /AKA/ @pool_retire_max_epoch@, @e_max@.
  , protocolParametersStakePoolVotingThresholds :: !StakePoolVotingThresholds
  , protocolParametersTreasuryExpansion               :: !MaestroRational
  -- ^ The fraction of the fee pot each epoch that goes to the treasury. This is the \"tau\" incentives parameter from the design document, /AKA/ @treasury_cut@.
  , protocolParametersVersion                         :: !ProtocolVersion
  -- ^ See `ProtocolVersion`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "protocolParameters", CamelToSnake]] ProtocolParameters

------------------------------------------------------------------
--  Types for @/chain-tip@ endpoint.
------------------------------------------------------------------

-- | Details about the most recently adopted block.
data ChainTip = ChainTip
  { chainTipBlockHash :: !BlockHash
  -- ^ Hash of this most recent block.
  , chainTipSlot      :: !SlotNo
  -- ^ Slot number for this most recent block.
  , chainTipHeight    :: !Word64
  -- ^ Block number (height) of this most recent block.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "chainTip", CamelToSnake]] ChainTip

-- | Timestamped `ChainTip` response.
data TimestampedChainTip = TimestampedChainTip
  { timestampedChainTipData        :: !ChainTip
  -- ^ See `ChainTip`.
  , timestampedChainTipLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedChainTip", CamelToSnake]] TimestampedChainTip

instance IsTimestamped TimestampedChainTip where
  type TimestampedData TimestampedChainTip = ChainTip
  getTimestampedData = timestampedChainTipData
  getTimestamp = timestampedChainTipLastUpdated
