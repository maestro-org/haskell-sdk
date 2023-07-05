module Maestro.Types.V0.Pool
  ( ActiveStake (..),
    DelegatorInfo (..),
    PoolListInfo (..),
    PoolBlock (..),
    PoolHistory (..),
    PoolInfo (..),
    PoolMetadata (..),
    PoolMetaJson (..),
    PoolRelay (..),
    Relay (..),
    PoolUpdate (..),
    PoolId,
  )
where

import           Data.Text               (Text)
import           Data.Time.Clock.POSIX   (POSIXTime)
import           Deriving.Aeson
import           GHC.Natural             (Natural)
import           Maestro.Types.V0.Common

data PoolId

data Reward

data Stake

data VrfKey

data PoolMeta

data PoolUpdateTx

data PoolListInfo = PoolListInfo
  { _poolListInfoPoolIdBech32 :: !(Bech32StringOf PoolId),
    _poolListInfoTicker       :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolListInfo", CamelToSnake]] PoolListInfo

data PoolBlock = PoolBlock
  { -- | Absolute slot of the block
    _poolBlkAbsSlot     :: !(Maybe AbsoluteSlot),
    -- | The Block Hash
    _poolBlkBlockHash   :: !BlockHash,
    -- | The Block Height i.e Block Number
    _poolBlkBlockHeight :: !BlockHeight,
    -- | UNIX timestamp when the block was mined
    _poolBlkBlockTime   :: !POSIXTime,
    -- | Epoch number
    _poolBlkEpochNo     :: !(Maybe EpochNo),
    -- | Epoch Slot
    _poolBlkEpochSlot   :: !(Maybe EpochSize)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolBlk", CamelToSnake]] PoolBlock

-- | Information summary of a delegator
data DelegatorInfo = DelegatorInfo
  { -- | Epoch at which the delegation becomes active
    _delegatorActiveEpochNo          :: !(Maybe EpochNo),
    -- | Delegator live stake
    _delegatorAmount                 :: !(Maybe Natural),
    -- | Transaction hash relating to the most recent delegation
    _delegatorLatestDelegationTxHash :: !(Maybe TxHash),
    -- | Bech32 encoded stake address (reward address)
    _delegatorStakeAddress           :: !(Maybe (Bech32StringOf Stake))
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_delegator", CamelToSnake]] DelegatorInfo

newtype ActiveStake = ActiveStake {unActiveStake :: Natural}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Real, Integral, FromJSON, ToJSON)

-- | Per-epoch history of a stake pool
data PoolHistory = PoolHistory
  { -- | Active stake in the epoch
    _poolHstActiveStake    :: !(Maybe ActiveStake),
    -- | Pool active stake as percentage of total active stake
    _poolHstActiveStakePct :: !(Maybe Text),
    -- | Blocks created in the epoch
    _poolHstBlockCnt       :: !(Maybe Natural),
    -- | Total rewards earned by pool delegators for the epoch
    _poolHstDelegRewards   :: !Natural,
    -- | Total Delegators in the epoch
    _poolHstDelegatorCnt   :: !(Maybe Natural),
    -- | The Epoch number
    _poolHstEpochNo        :: !EpochNo,
    -- | Annual return percentage for delegators for the epoch
    _poolHstEpochRos       :: !Text,
    -- | Pool fixed cost
    _poolHstFixedCost      :: !Natural,
    -- | Pool margin
    _poolHstMargin         :: !(Maybe Double),
    -- | Fees collected for the epoch
    _poolHstPoolFees       :: !Natural,
    -- | Pool saturation percent
    _poolHstSaturationPct  :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolHst", CamelToSnake]] PoolHistory

data Relay = Relay
  { _relayDns  :: !(Maybe Text),
    _relayIpv4 :: !(Maybe Text),
    _relayIpv6 :: !(Maybe Text),
    _relayPort :: !(Maybe Int),
    _relaySrv  :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_relay", CamelToSnake]] Relay

-- | JSON metadata associated with a stake pool
data PoolMetaJson = PoolMetaJson
  { -- | Pool description
    _poolMetaJsonDescription :: !(Maybe Text),
    -- | Pool home page URL
    _poolMetaJsonHomepage    :: !(Maybe Text),
    -- | Pool name
    _poolMetaJsonName        :: !Text,
    -- | Pool ticker symbol
    _poolMetaJsonTicker      :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolMetaJson", CamelToSnake]] PoolMetaJson

-- | Metadata associated with a stake pool
data PoolMetadata = PoolMetadata
  { -- | Hash of the pool metadata
    _poolMetadataMetaHash     :: !(Maybe Text),
    -- | JSON metadata associated with a stake pool
    _poolMetadataMetaJson     :: !(Maybe PoolMetaJson),
    -- | URL pointing to the pool metadata
    _poolMetadataMetaUrl      :: !(Maybe Text),
    -- | Bech32 encoded pool ID
    _poolMetadataPoolIdBech32 :: !(Bech32StringOf PoolId)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolMetadata", CamelToSnake]] PoolMetadata

data PoolInfo = PoolInfo
  { -- | Epoch when the update takes effect
    _poolInfActiveEpochNo  :: !EpochNo,
    -- | Active stake
    _poolInfActiveStake    :: !(Maybe Natural),
    -- | Number of blocks created
    _poolInfBlockCount     :: !(Maybe Natural),
    -- | Pool fixed cost
    _poolInfFixedCost      :: !Natural,
    -- | Number of current delegators
    _poolInfLiveDelegators :: !Natural,
    -- | Account balance of pool owners
    _poolInfLivePledge     :: !(Maybe Natural),
    -- | Live saturation
    _poolInfLiveSaturation :: !(Maybe Text),
    -- | Live stake
    _poolInfLiveStake      :: !(Maybe Integer),
    -- | Pool margin
    _poolInfMargin         :: !Double,
    -- | Hash of the pool metadata
    _poolInfMetaHash       :: !(Maybe Text),
    -- | JSON metadata associated with a stake pool
    _poolInfMetaJson       :: !(Maybe PoolMetaJson),
    -- | URL pointing to the pool metadata
    _poolInfMetaUrl        :: !(Maybe Text),
    -- | Pool operational certificate
    _poolInfOpCert         :: !(Maybe Text),
    -- | Operational certificate counter
    _poolInfOpCertCounter  :: !(Maybe Integer),
    -- | List of stake keys which control the pool
    _poolInfOwners         :: ![Text],
    -- | Pool pledge
    _poolInfPledge         :: !Integer,
    -- | Bech32 encoded pool ID
    _poolInfPoolIdBech32   :: !(Bech32StringOf PoolId),
    -- | Hex encoded pool ID
    _poolInfPoolIdHex      :: !(HexStringOf PoolId),
    -- | Status of the pool
    _poolInfPoolStatus     :: !(Maybe Text),
    -- | Stake pool relay
    _poolInfRelays         :: ![Relay],
    -- | Epoch at which the pool will be retired
    _poolInfRetiringEpoch  :: !(Maybe EpochNo),
    -- | Reward address associated with the pool
    _poolInfRewardAddr     :: !(Maybe (Bech32StringOf PoolId)),
    -- | Pool stake share
    _poolInfSigma          :: !(Maybe Text),
    -- | VRF key hash
    _poolInfVrfKeyHash     :: !(HashStringOf VrfKey)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolInf", CamelToSnake]] PoolInfo

-- | Relay declared by a stake pool
data PoolRelay = PoolRelay
  { _poolRelPoolIdBech32 :: !(Bech32StringOf PoolId),
    _poolRelRelays       :: ![Relay]
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolRel", CamelToSnake]] PoolRelay

-- | Update to a stake pool
data PoolUpdate = PoolUpdate
  { -- | Epoch when the update takes effect
    _poolUpdateActiveEpochNo :: !EpochNo,
    -- | UNIX timestamp of the block containing the transaction
    _poolUpdateBlockTime     :: !(Maybe POSIXTime),
    -- | Pool fixed cost
    _poolUpdateFixedCost     :: !Natural,
    -- | Pool margin
    _poolUpdateMargin        :: !Double,
    -- | Hash of the pool metadata
    _poolUpdateMetaHash      :: !(Maybe (HashStringOf PoolMeta)),
    -- | JSON metadata associated with a stake pool
    _poolUpdateMetaJson      :: !(Maybe PoolMetaJson),
    -- | URL pointing to the pool metadata
    _poolUpdateMetaUrl       :: !(Maybe Text),
    -- | List of stake keys which control the pool
    _poolUpdateOwners        :: ![Text],
    -- | Pool pledge
    _poolUpdatePledge        :: !Integer,
    -- | Bech32 encoded pool ID
    _poolUpdatePoolIdBech32  :: !(Bech32StringOf PoolId),
    -- | Hex encoded pool ID
    _poolUpdatePoolIdHex     :: !(HexStringOf PoolId),
    -- | Status of the pool
    _poolUpdatePoolStatus    :: !(Maybe Text),
    -- | Stake pool relay
    _poolUpdateRelays        :: ![Relay],
    -- | Epoch at which the pool will be retired
    _poolUpdateRetiringEpoch :: !(Maybe Text),
    -- | Reward address associated with the pool
    _poolUpdateRewardAddr    :: !(Maybe (Bech32StringOf Reward)),
    -- | Transaction hash for the transaction which contained the update
    _poolUpdateTxHash        :: !(HashStringOf PoolUpdateTx),
    -- | VRF key hash
    _poolUpdateVrfKeyHash    :: !(HashStringOf VrfKey)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolUpdate", CamelToSnake]] PoolUpdate
