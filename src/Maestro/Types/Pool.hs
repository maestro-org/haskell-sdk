module Maestro.Types.Pool
  ( ActiveStake(..)
  , DelegatorInfo(..)
  , Pool(..)
  , PoolBlock(..)
  , PoolHistory(..)
  , PoolInfo(..)
  , PoolMetadata(..)
  , PoolMetaJson(..)
  , PoolRelay(..)
  , PoolRelayInfo(..)
  , PoolUpdates(..)
  ) where

import           Data.Text             (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Deriving.Aeson
import           GHC.Natural           (Natural)
import           Maestro.Types.Common

data PoolId
data Reward
data Stake
data VrfKey
data PoolMeta
data PoolUpdateTx

data Pool = Pool
  { _plPoolIdBech32 :: !(Bech32StringOf PoolId)
  , _plTicker       :: !(Maybe String)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_pl", CamelToSnake]] Pool

data PoolBlock = PoolBlock
  { _poolBlkAbsSlot     :: !(Maybe AbsoluteSlot)  -- ^ Absolute slot of the block
  , _poolBlkBlockHash   :: !BlockHash             -- ^ The Block Hash
  , _poolBlkBlockHeight :: !BlockHeight           -- ^ The Block Height i.e Block Number
  , _poolBlkBlockTime   :: !POSIXTime             -- ^ UNIX timestamp when the block was mined
  , _poolBlkEpochNo     :: !(Maybe EpochNo)       -- ^ Epoch number
  , _poolBlkEpochSlot   :: !(Maybe EpochSize)     -- ^ Epoch Slot
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolBlk", CamelToSnake]] PoolBlock

-- | Information summary of a delegator
data DelegatorInfo = DelegatorInfo
  { _delegatorActiveEpochNo          :: !(Maybe EpochNo)
  -- ^ Epoch at which the delegation becomes active
  , _delegatorAmount                 :: !(Maybe Natural)
  -- ^ Delegator live stake
  , _delegatorLatestDelegationTxHash :: !(Maybe TxHash)
  -- ^ Transaction hash relating to the most recent delegation
  , _delegatorStakeAddress           :: !(Maybe (Bech32StringOf Stake))
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
  { _poolHstActiveStake    :: !(Maybe ActiveStake)
  -- ^ Active stake in the epoch
  , _poolHstActiveStakePct :: !(Maybe String)
  -- ^ Pool active stake as percentage of total active stake
  , _poolHstBlockCnt       :: !(Maybe Natural)
  -- ^ Blocks created in the epoch
  , _poolHstDelegRewards   :: !Natural
  -- ^ Total rewards earned by pool delegators for the epoch
  , _poolHstDelegatorCnt   :: !(Maybe Natural)
  -- ^ Total Delegators in the epoch
  , _poolHstEpochNo        :: !EpochNo
  -- ^ The Epoch number
  , _poolHstEpochRos       :: !String
  -- ^ Annual return percentage for delegators for the epoch
  , _poolHstFixedCost      :: !Natural
  -- ^ Pool fixed cost
  , _poolHstMargin         :: !(Maybe Double)
  -- ^ Pool margin
  , _poolHstPoolFees       :: !Natural
  -- ^ Fees collected for the epoch
  , _poolHstSaturationPct  :: !(Maybe String)
  -- ^ Pool saturation percent
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolHst", CamelToSnake]] PoolHistory

data PoolRelayInfo = PoolRelayInfo
  { _poolRelInfoDns  :: !(Maybe String)
  , _poolRelInfoIpv4 :: !(Maybe String)
  , _poolRelInfoIpv6 :: !(Maybe String)
  , _poolRelInfoPort :: !(Maybe Int)
  , _poolRelInfoSrv  :: !(Maybe String)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolRelInfo", CamelToSnake]] PoolRelayInfo

-- | JSON metadata associated with a stake pool
data PoolMetaJson  = PoolMetaJson
  { _poolMetaJsonDescription :: !(Maybe Text)   -- ^ Pool description
  , _poolMetaJsonHomepage    :: !(Maybe String) -- ^ Pool home page URL
  , _poolMetaJsonName        :: !String         -- ^ Pool name
  , _poolMetaJsonTicker      :: !(Maybe String) -- ^ Pool ticker symbol
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolMetaJson", CamelToSnake]] PoolMetaJson

-- | Metadata associated with a stake pool
data PoolMetadata = PoolMetadata
  { _poolMetadataMetaHash     :: !(Maybe String)
  -- ^ Hash of the pool metadata
  , _poolMetadataMetaJson     :: !(Maybe PoolMetaJson)
  -- ^ JSON metadata associated with a stake pool
  , _poolMetadataMetaUrl      :: !(Maybe String)
  -- ^ URL pointing to the pool metadata
  , _poolMetadataPoolIdBech32 :: !(Bech32StringOf PoolId)
  -- ^ Bech32 encoded pool ID
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolMetadata", CamelToSnake]] PoolMetadata

data PoolInfo = PoolInfo
  { _poolInfActiveEpochNo  :: !EpochNo
  -- ^ Epoch when the update takes effect
  , _poolInfActiveStake    :: !(Maybe Natural)
  -- ^ Active stake
  , _poolInfBlockCount     :: !(Maybe Natural)
  -- ^ Number of blocks created
  , _poolInfFixedCost      :: !Natural
  -- ^ Pool fixed cost
  , _poolInfLiveDelegators :: !Natural
  -- ^ Number of current delegators
  , _poolInfLivePledge     :: !(Maybe Natural)
  -- ^ Account balance of pool owners
  , _poolInfLiveSaturation :: !(Maybe String)
  -- ^ Live saturation
  , _poolInfLiveStake      :: !(Maybe Integer)
  -- ^ Live stake
  , _poolInfMargin         :: !Double
  -- ^ Pool margin
  , _poolInfMetaHash       :: !(Maybe String)
  -- ^ Hash of the pool metadata
  , _poolInfMetaJson       :: !(Maybe PoolMetaJson)
  -- ^ JSON metadata associated with a stake pool
  , _poolInfMetaUrl        :: !(Maybe String)
  -- ^ URL pointing to the pool metadata
  , _poolInfOpCert         :: !(Maybe String)
  -- ^ Pool operational certificate
  , _poolInfOpCertCounter  :: !(Maybe Integer)
  -- ^ Operational certificate counter
  , _poolInfOwners         :: ![String]
  -- ^ List of stake keys which control the pool
  , _poolInfPledge         :: !Integer
  -- ^ Pool pledge
  , _poolInfPoolIdBech32   :: !(Bech32StringOf PoolId)
  -- ^ Bech32 encoded pool ID
  , _poolInfPoolIdHex      :: !(HexStringOf PoolId)
  -- ^ Hex encoded pool ID
  , _poolInfPoolStatus     :: !(Maybe String)
  -- ^ Status of the pool
  , _poolInfRelays         :: ![PoolRelayInfo]
  -- ^ Stake pool relay
  , _poolInfRetiringEpoch  :: !(Maybe EpochNo)
  -- ^ Epoch at which the pool will be retired
  , _poolInfRewardAddr     :: !(Maybe (Bech32StringOf PoolId))
  -- ^ Reward address associated with the pool
  , _poolInfSigma          :: !(Maybe String)
  -- ^ Pool stake share
  , _poolInfVrfKeyHash     :: !(HashStringOf VrfKey)
  -- ^ VRF key hash
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolInf", CamelToSnake]] PoolInfo

-- | Relay declared by a stake pool
data PoolRelay = PoolRelay
  { _poolRelPoolIdBech32 :: !(Bech32StringOf PoolId)
  , _poolRelRelays       :: ![PoolRelayInfo]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolRel", CamelToSnake]] PoolRelay

-- | Update to a stake pool
data PoolUpdates = PoolUpdates
  { _poolUpdateActiveEpochNo :: !EpochNo
  -- ^ Epoch when the update takes effect
  , _poolUpdateBlockTime     :: !(Maybe POSIXTime)
  -- ^ UNIX timestamp of the block containing the transaction
  , _poolUpdateFixedCost     :: !Natural
  -- ^ Pool fixed cost
  , _poolUpdateMargin        :: !Double
  -- ^ Pool margin
  , _poolUpdateMetaHash      :: !(Maybe (HashStringOf PoolMeta))
  -- ^ Hash of the pool metadata
  , _poolUpdateMetaJson      :: !(Maybe PoolMetaJson)
  -- ^ JSON metadata associated with a stake pool
  , _poolUpdateMetaUrl       :: !(Maybe String)
  -- ^ URL pointing to the pool metadata
  , _poolUpdateOwners        :: ![String]
  -- ^ List of stake keys which control the pool
  , _poolUpdatePledge        :: !Integer
  -- ^ Pool pledge
  , _poolUpdatePoolIdBech32  :: !(Bech32StringOf PoolId)
  -- ^ Bech32 encoded pool ID
  , _poolUpdatePoolIdHex     :: !(HexStringOf PoolId)
  -- ^ Hex encoded pool ID
  , _poolUpdatePoolStatus    :: !(Maybe String)
  -- ^ Status of the pool
  , _poolUpdateRelays        :: ![PoolRelayInfo]
  -- ^ Stake pool relay
  , _poolUpdateRetiringEpoch :: !(Maybe String)
  -- ^ Epoch at which the pool will be retired
  , _poolUpdateRewardAddr    :: !(Maybe (Bech32StringOf Reward))
  -- ^ Reward address associated with the pool
  , _poolUpdateTxHash        :: !(HashStringOf PoolUpdateTx)
  -- ^ Transaction hash for the transaction which contained the update
  , _poolUpdateVrfKeyHash    :: !(HashStringOf VrfKey)
  -- ^ VRF key hash
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolUpdate", CamelToSnake]] PoolUpdates
