-- | Module to define types for /\"Accounts\"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/accounts).
module Maestro.Types.V1.Accounts
  ( PaginatedAddress (..)
  , PaginatedAsset (..)
  , AccountInfo (..)
  , TimestampedAccountInfo (..)
  , AccountHistory (..)
  , PaginatedAccountHistory (..)
  , AccountStakingRewardType (..)
  , AccountReward (..)
  , PaginatedAccountReward (..)
  , AccountUpdate (..)
  , PaginatedAccountUpdate (..)
  ) where

import           Data.Word               (Word64)
import           Deriving.Aeson
import           Maestro.Types.V1.Common (Address, Asset, Bech32StringOf,
                                          EpochNo, HasCursor (..),
                                          IsTimestamped (..), LastUpdated,
                                          LowerFirst, NextCursor, PoolId,
                                          RewardAddress, SlotNo, TxHash)

-- | Paginated list of addresses.
data PaginatedAddress = PaginatedAddress
  { paginatedAddressData        :: ![Bech32StringOf Address]
  -- ^ Desired data of list of addresses.
  , paginatedAddressLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  , paginatedAddressNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedAddress", CamelToSnake]] PaginatedAddress

instance IsTimestamped PaginatedAddress where
  type TimestampedData PaginatedAddress = [Bech32StringOf Address]
  getTimestampedData = paginatedAddressData
  getTimestamp = paginatedAddressLastUpdated

instance HasCursor PaginatedAddress where
  getNextCursor = paginatedAddressNextCursor

-- | Paginated list of assets.
data PaginatedAsset = PaginatedAsset
  { paginatedAssetData        :: ![Asset]
  -- ^ Desired data of list of assets.
  , paginatedAssetLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  , paginatedAssetNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedAsset", CamelToSnake]] PaginatedAsset

instance IsTimestamped PaginatedAsset where
  type TimestampedData PaginatedAsset = [Asset]
  getTimestampedData = paginatedAssetData
  getTimestamp = paginatedAssetLastUpdated

instance HasCursor PaginatedAsset where
  getNextCursor = paginatedAssetNextCursor

-- | Summary of information regarding a stake account.
data AccountInfo = AccountInfo
  { accountInfoDelegatedPool    :: !(Maybe (Bech32StringOf PoolId))
  -- ^ Bech32 Pool ID that the stake key is delegated to.
  , accountInfoRegistered       :: !Bool
  -- ^ True if the stake key is registered.
  , accountInfoRewardsAvailable :: !Word64
  -- ^ The amount of rewards that are available to be withdrawn.
  , accountInfoStakeAddress     :: !(Bech32StringOf RewardAddress)
  -- ^ Bech32 encoded stake address.
  , accountInfoTotalBalance     :: !Word64
  -- ^ Total balance controlled by the stake key (sum of UTxO and rewards).
  , accountInfoTotalRewarded    :: !Word64
  -- ^ Total rewards earned.
  , accountInfoTotalWithdrawn   :: !Word64
  -- ^ Total rewards withdrawn.
  , accountInfoUtxoBalance      :: !Word64
  -- ^ Amount locked in UTxOs controlled by addresses with the stake key.
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "accountInfo", CamelToSnake]] AccountInfo

-- | Timestamped `AccountInfo` response.
data TimestampedAccountInfo = TimestampedAccountInfo
  { timestampedAccountInfoData        :: !AccountInfo
  -- ^ See `AccountInfo`.
  , timestampedAccountInfoLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedAccountInfo", CamelToSnake]] TimestampedAccountInfo

instance IsTimestamped TimestampedAccountInfo where
  type TimestampedData TimestampedAccountInfo = AccountInfo
  getTimestampedData = timestampedAccountInfoData
  getTimestamp = timestampedAccountInfoLastUpdated

-- | Per-epoch information about a stake account.
data AccountHistory = AccountHistory
  { accountHistoryActiveStake :: !Word64
  , accountHistoryEpochNo     :: !EpochNo
  , accountHistoryPoolId      :: !(Maybe (Bech32StringOf PoolId))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "accountHistory", CamelToSnake]] AccountHistory

-- | Paginated `AccountHistory`.
data PaginatedAccountHistory = PaginatedAccountHistory
  { paginatedAccountHistoryData        :: ![AccountHistory]
  -- ^ Desired data.
  , paginatedAccountHistoryLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  , paginatedAccountHistoryNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedAccountHistory", CamelToSnake]] PaginatedAccountHistory

instance IsTimestamped PaginatedAccountHistory where
  type TimestampedData PaginatedAccountHistory = [AccountHistory]
  getTimestampedData = paginatedAccountHistoryData
  getTimestamp = paginatedAccountHistoryLastUpdated

instance HasCursor PaginatedAccountHistory where
  getNextCursor = paginatedAccountHistoryNextCursor

-- | Staking-related reward type.
data AccountStakingRewardType = Member | Leader | Refund
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] AccountStakingRewardType

-- | Staking rewards for the specified stake key (pool-member and pool-leader rewards, deposit refunds).
data AccountReward = AccountReward
  { accountRewardAmount         :: !Word64
  -- ^ Reward amount.
  , accountRewardEarnedEpoch    :: !EpochNo
  -- ^ Epoch in which the reward was earned.
  , accountRewardPoolId         :: !(Bech32StringOf PoolId)
  -- ^ Bech32 encoded pool ID (if relevant to reward type).
  , accountRewardSpendableEpoch :: !EpochNo
  -- ^ Epoch at which the reward is spendable.
  , accountRewardType           :: !AccountStakingRewardType
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "accountReward", CamelToSnake]] AccountReward

-- | Paginated `AccountReward`.
data PaginatedAccountReward = PaginatedAccountReward
  { paginatedAccountRewardData        :: ![AccountReward]
  -- ^ See `AccountReward`.
  , paginatedAccountRewardLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  , paginatedAccountRewardNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedAccountReward", CamelToSnake]] PaginatedAccountReward

instance IsTimestamped PaginatedAccountReward where
  type TimestampedData PaginatedAccountReward = [AccountReward]
  getTimestampedData = paginatedAccountRewardData
  getTimestamp = paginatedAccountRewardLastUpdated

instance HasCursor PaginatedAccountReward where
  getNextCursor = paginatedAccountRewardNextCursor

-- | Type of staking-related action.
data AccountAction = Registration | Deregistration | Delegation | Withdrawal
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] AccountAction

-- | Stake account related update.
data AccountUpdate = AccountUpdate
  { accountUpdateAbsSlot :: !SlotNo
  -- ^ Absolute slot of the block which contained the transaction.
  , accountUpdateAction  :: !AccountAction
  -- ^ See `AccountAction`.
  , accountUpdateEpoch   :: !EpochNo
  -- ^ Epoch number in which the transaction occured.
  , accountUpdateTxHash  :: !TxHash
  -- ^ Transaction hash of the transaction which performed the action.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "accountUpdate", CamelToSnake]] AccountUpdate

-- | Paginated `AccountUpdate`.
data PaginatedAccountUpdate = PaginatedAccountUpdate
  { paginatedAccountUpdateData        :: ![AccountUpdate]
  -- ^ See `AccountUpdate`.
  , paginatedAccountUpdateLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  , paginatedAccountUpdateNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedAccountUpdate", CamelToSnake]] PaginatedAccountUpdate

instance IsTimestamped PaginatedAccountUpdate where
  type TimestampedData PaginatedAccountUpdate = [AccountUpdate]
  getTimestampedData = paginatedAccountUpdateData
  getTimestamp = paginatedAccountUpdateLastUpdated

instance HasCursor PaginatedAccountUpdate where
  getNextCursor = paginatedAccountUpdateNextCursor
