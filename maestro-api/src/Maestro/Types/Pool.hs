module Maestro.Types.Pool where

import           Data.Text             (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Deriving.Aeson
import           GHC.Natural           (Natural)
import           Maestro.Types.Common


data Pool = Pool
  { _plPoolIdBech32 :: !String
  , _plTicker       :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_pl", CamelToSnake]] Pool

data PoolBlock = PoolBlock
  { _poolBlkAbsSlot     :: !AbsoluteSlot  -- ^ Absolute slot of the block
  , _poolBlkBlockHash   :: !BlockHash     -- ^ The Block Hash
  , _poolBlkBlockHeight :: !BlockHeight   -- ^ The Block Height i.e Block Number
  , _poolBlkBlockTime   :: !POSIXTime     -- ^ UNIX timestamp when the block was mined
  , _poolBlkEpochNo     :: !EpochNo       -- ^ Epoch number
  , _poolBlkEpochSlot   :: !EpochSlot     -- ^ Epoch Slot
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolBlk", CamelToSnake]] PoolBlock

-- | Information summary of a delegator
data DelegatorInfo = DelegatorInfo
  { _delegatorActiveEpochNo          :: !EpochNo
  -- ^ Epoch at which the delegation becomes active
  , _delegatorAmount                 :: !Natural
  -- ^ Delegator live stake
  , _delegatorLatestDelegationTxHash :: !String
  -- ^ Transaction hash relating to the most recent delegation
  , _delegatorStakeAddress           :: !String
  -- ^ Bech32 encoded stake address (reward address)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_delegator", CamelToSnake]] DelegatorInfo


newtype ActiveStake = ActiveStake {unActiveStake :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

-- | Per-epoch history of a stake pool
data PoolHistory = PoolHistory
  { _poolHstActiveStake    :: !ActiveStake
  -- ^ Active stake in the epoch
  , _poolHstActiveStakePct :: !String
  -- ^ Pool active stake as percentage of total active stake
  , _poolHstBlockCnt       :: !Natural
  -- ^ Blocks created in the epoch
  , _poolHstDelegRewards   :: !Natural
  -- ^ Total rewards earned by pool delegators for the epoch
  , _poolHstDelegatorCnt   :: !Natural
  -- ^ Total Delegators in the epoch
  , _poolHstEpochNo        :: !EpochNo
  -- ^ The Epoch number
  , _poolHstEpochRos       :: !String
  -- ^ Annual return percentage for delegators for the epoch
  , _poolHstFixedCost      :: !Integer
  -- ^ Pool fixed cost
  , _poolHstMargin         :: !Double
  -- ^ Pool margin
  , _poolHstPoolFees       :: !Integer
  -- ^ Fees collected for the epoch
  , _poolHstSaturationPct  :: !String
  -- ^ Pool saturation percent
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolHst", CamelToSnake]] PoolHistory

data PoolRelayInfo = PoolRelayInfo
  { _poolRelInfoDns  :: !(Maybe String)
  , _poolRelInfoIpv4 :: !(Maybe String)
  , _poolRelInfoIpv6 :: !(Maybe String)
  , _poolRelInfoPort :: !Int
  , _poolRelInfoSrv  :: !(Maybe String)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolRelInfo", CamelToSnake]] PoolRelayInfo

-- | JSON metadata associated with a stake pool
data PoolMetaJson  = PoolMetaJson
  { _poolMetaJsonDescription :: !Text   -- ^ Pool description
  , _poolMetaJsonHomepage    :: !String -- ^ Pool home page URL
  , _poolMetaJsonName        :: !String -- ^ Pool name
  , _poolMetaJsonTicker      :: !String -- ^ Pool ticker symbol
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolMetaJson", CamelToSnake]] PoolMetaJson

-- | Metadata associated with a stake pool
data PoolMetadata = PoolMetadata
  { _poolMetadataMetaHash     :: !String
  -- ^ Hash of the pool metadata
  , _poolMetadataMetaJson     :: !PoolMetaJson
  -- ^ JSON metadata associated with a stake pool
  , _poolMetadataMetaUrl      :: !String
  -- ^ URL pointing to the pool metadata
  , _poolMetadataPoolIdBech32 :: !String
  -- ^ Bech32 encoded pool ID
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolMetadata", CamelToSnake]] PoolMetadata

data PoolInfo = PoolInfo
  { _poolInfActiveEpochNo  :: !EpochNo
  -- ^ Epoch when the update takes effect
  , _poolInfActiveStake    :: !Natural
  -- ^ Active stake
  , _poolInfBlockCount     :: !Natural
  -- ^ Number of blocks created
  , _poolInfFixedCost      :: !Natural
  -- ^ Pool fixed cost
  , _poolInfLiveDelegators :: !Natural
  -- ^ Number of current delegators
  , _poolInfLivePledge     :: !Natural
  -- ^ Account balance of pool owners
  , _poolInfLiveSaturation :: !String
  -- ^ Live saturation
  , _poolInfLiveStake      :: !Integer
  -- ^ Live stake
  , _poolInfMargin         :: !Double
  -- ^ Pool margin
  , _poolInfMetaHash       :: !String
  -- ^ Hash of the pool metadata
  , _poolInfMetaJson       :: !PoolMetaJson
  -- ^ JSON metadata associated with a stake pool
  , _poolInfMetaUrl        :: !String
  -- ^ URL pointing to the pool metadata
  , _poolInfOpCert         :: !String
  -- ^ Pool operational certificate
  , _poolInfOpCertCounter  :: !Integer
  -- ^ Operational certificate counter
  , _poolInfOwners         :: ![String]
  -- ^ List of stake keys which control the pool
  , _poolInfPledge         :: !Integer
  -- ^ Pool pledge
  , _poolInfPoolIdBech32   :: !String
  -- ^ Bech32 encoded pool ID
  , _poolInfPoolIdHex      :: !String
  -- ^ Hex encoded pool ID
  , _poolInfPoolStatus     :: !String
  -- ^ Status of the pool
  , _poolInfRelays         :: ![PoolRelayInfo]
  -- ^ Stake pool relay
  , _poolInfRetiringEpoch  :: !(Maybe EpochNo)
  -- ^ Epoch at which the pool will be retired
  , _poolInfRewardAddr     :: !String
  -- ^ Reward address associated with the pool
  , _poolInfSigma          :: !String
  -- ^ Pool stake share
  , _poolInfVrfKeyHash     :: !String
  -- VRF key hash
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolInf", CamelToSnake]] PoolInfo

-- | Relay declared by a stake pool
data PoolRelay = PoolRelay
  { _poolRelPoolIdBech32 :: !String
  , _poolRelRelays       :: ![PoolRelayInfo]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolRel", CamelToSnake]] PoolRelay

-- | Update to a stake pool
data PoolUpdates = PoolUpdates
  { _poolUpdateActiveEpochNo :: !EpochNo
  -- ^ Epoch when the update takes effect
  , _poolUpdateBlockTime     :: !POSIXTime
  -- ^ UNIX timestamp of the block containing the transaction
  , _poolUpdateFixedCost     :: !Natural
  -- ^ Pool fixed cost
  , _poolUpdateMargin        :: !Double
  -- ^ Pool margin
  , _poolUpdateMetaHash      :: !String
  -- ^ Hash of the pool metadata
  , _poolUpdateMetaJson      :: !(Maybe PoolMetaJson)
  -- ^ JSON metadata associated with a stake pool
  , _poolUpdateMetaUrl       :: !String
  -- ^ URL pointing to the pool metadata
  , _poolUpdateOwners        :: ![String]
  -- ^ List of stake keys which control the pool
  , _poolUpdatePledge        :: !Integer
  -- ^ Pool pledge
  , _poolUpdatePoolIdBech32  :: !String
  -- ^ Bech32 encoded pool ID
  , _poolUpdatePoolIdHex     :: !String
  -- ^ Hex encoded pool ID
  , _poolUpdatePoolStatus    :: !String
  -- ^ Status of the pool
  , _poolUpdateRelays        :: ![PoolRelayInfo]
  -- ^ Stake pool relay
  , _poolUpdateRetiringEpoch :: !(Maybe String)
  -- ^ Epoch at which the pool will be retired
  , _poolUpdateRewardAddr    :: !String
  -- ^ Reward address associated with the pool
  , _poolUpdateTxHash        :: !String
  -- ^ Transaction hash for the transaction which contained the update
  , _poolUpdateVrfKeyHash    :: !String
  -- ^ VRF key hash
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolUpdate", CamelToSnake]] PoolUpdates
