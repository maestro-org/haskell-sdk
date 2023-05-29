module Maestro.Types.Accounts
  ( AccountInfo (..)
  , Asset (..)
  , AccountHistory (..)
  , AccountReward (..)
  , AccountUpdate (..)
  ) where

import           Deriving.Aeson

-- | Information about an account
data AccountInfo = AccountInfo
  { _accountInfoDelegatedPool    :: !String
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
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountInfo", CamelToSnake]] AccountInfo


-- | Information about an Account Assets
data Asset = Asset
  { _assetQuantity :: !Integer
  , _assetUnit     :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_asset", CamelToSnake]] Asset

-- | Information about an Account Assets
data AccountHistory = AccountHistory
  { _accountHistoryActiveStake :: !Integer
  , _accountHistoryEpochNo     :: !Integer
  , _accountHistoryPoolId      :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountHistory", CamelToSnake]] AccountHistory

-- | Information about an Account Assets
data AccountReward = AccountReward
  { _accountRewardAmount         :: !Integer
  , _accountRewardEarnedEpoch    :: !Integer
  , _accountRewardPoolId         :: !String
  , _accountRewardSpendableEpoch :: !Integer
  , _accountRewardType           :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountReward", CamelToSnake]] AccountReward

-- | Information about an Account Assets
data AccountUpdate = AccountUpdate
  { _accountUpdateAbsSlot :: !Integer
  , _accountUpdateAction  :: !String
  , _accountUpdateEpoch   :: !Integer
  , _accountUpdateTxHash  :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountUpdate", CamelToSnake]] AccountUpdate
