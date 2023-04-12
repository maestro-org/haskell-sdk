module Maestro.Types.Accounts where

import           Deriving.Aeson

-- | Information about an account
data AccountsInfo = AccountsInfo
  {  _accountInfoDelegatedPool   :: !String
  , _accountInfoRegistered       :: !Bool
  , _accountInfoRewardsAvailable :: !Integer
  , _accountInfoStakeAddress     :: !String
  , _accountInfoTotalBalance     :: !Integer
  , _accountInfoTotalRewarded    :: !Integer
  , _accountInfoTotalWithdrawn   :: !Integer
  , _accountInfoUtxoBalance      :: !Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountInfo", CamelToSnake]] AccountsInfo


-- | Information about an Account Assets
data AccountsAssets = AccountsAssets
  { _accountAssetQuantity :: !Integer
  , _accountAssetUnit     :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountAsset", CamelToSnake]] AccountsAssets

-- | Information about an Account Assets
data AccountsHistory = AccountsHistory
  { _accountHistoryActiveStake :: !Integer
  , _accountHistoryEpochNo     :: !Integer
  , _accountHistoryPoolId      :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountHistory", CamelToSnake]] AccountsHistory

-- | Information about an Account Assets
data AccountsRewards = AccountsRewards
  { _accountRewardAmount         :: !Integer
  , _accountRewardEarnedEpoch    :: !Integer
  , _accountRewardPoolId         :: !String
  , _accountRewardSpendableEpoch :: !Integer
  , _accountRewardType           :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountReward", CamelToSnake]] AccountsRewards

-- | Information about an Account Assets
data AccountsUpdates = AccountsUpdates
  { _accountUpdateAbsSlot :: !Integer
  , _accountUpdateAction  :: !String
  , _accountUpdateEpoch   :: !Integer
  , _accountUpdateTxHash  :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountUpdate", CamelToSnake]] AccountsUpdates
