module Maestro.Types.V0.Accounts
  ( AccountInfo (..)
  , AccountHistory (..)
  , AccountReward (..)
  , AccountUpdate (..)
  ) where

import           Data.Text               (Text)
import           Deriving.Aeson
import           Maestro.Types.V0.Common (EpochNo, LowerFirst, SlotNo)

-- | Information about an account
data AccountInfo = AccountInfo
  { _accountInfoDelegatedPool    :: !(Maybe Text)
  , _accountInfoRegistered       :: !Bool
  , _accountInfoRewardsAvailable :: !Integer
  , _accountInfoStakeAddress     :: !Text
  , _accountInfoTotalBalance     :: !Integer
  , _accountInfoTotalRewarded    :: !Integer
  , _accountInfoTotalWithdrawn   :: !Integer
  , _accountInfoUtxoBalance      :: !Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountInfo", CamelToSnake]] AccountInfo


-- | Information about an Account Assets
data AccountHistory = AccountHistory
  { _accountHistoryActiveStake :: !Integer
  , _accountHistoryEpochNo     :: !EpochNo
  , _accountHistoryPoolId      :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountHistory", CamelToSnake]] AccountHistory

-- | Information about an Account Assets
data AccountReward = AccountReward
  { _accountRewardAmount         :: !Integer
  , _accountRewardEarnedEpoch    :: !EpochNo
  , _accountRewardPoolId         :: !Text
  , _accountRewardSpendableEpoch :: !EpochNo
  , _accountRewardType           :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountReward", CamelToSnake]] AccountReward

data AccountAction = Registration | Deregistration | Delegation | Withdrawal
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] AccountAction

-- | Information about an Account Assets
data AccountUpdate = AccountUpdate
  { _accountUpdateAbsSlot :: !SlotNo
  , _accountUpdateAction  :: !AccountAction
  , _accountUpdateEpoch   :: !EpochNo
  , _accountUpdateTxHash  :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountUpdate", CamelToSnake]] AccountUpdate
