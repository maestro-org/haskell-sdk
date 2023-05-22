module Maestro.API.Accounts where

import           Maestro.Types.Accounts
import           Maestro.Types.Common
import           Maestro.Util.Pagination
import           Servant.API
import           Servant.API.Generic

data AccountsAPI route = AccountsAPI
  {
    _account
      :: route
      :- Capture "stake_addr" String
      :> Get '[JSON] AccountsInfo

  , _accountAddresses
      ::  route
      :- Capture "stake_addr" String
      :> "addresses"
      :> Pagination
      :> Get '[JSON] [String]

  , _accountAssets
      ::  route
      :- Capture "stake_addr" String
      :> "assets"
      :> Pagination
      :> Get  '[JSON] [AccountsAssets]

  , _accountsHistory
      ::  route
      :- Capture "stake_addr" String
      :> "history"
      :> QueryParam "epoch_no" EpochNo
      :> Pagination
      :> Get  '[JSON] [AccountsHistory]

  , _accountsReward
      ::  route
      :- Capture "stake_addr" String
      :> "rewards"
      :> Pagination
      :> Get  '[JSON] [AccountsRewards]

  , _accountsUpdates
      ::  route
      :- Capture "stake_addr" String
      :> "updates"
      :> Pagination
      :> Get  '[JSON] [AccountsUpdates]

  } deriving (Generic)
