module Maestro.API.Accounts where

import           Maestro.Types.Accounts
import           Maestro.Types.Common
import           Maestro.Client.Core.Pagination
import           Servant.API
import           Servant.API.Generic

data AccountsAPI route = AccountsAPI
  {
    _account
      :: route
      :- Capture "stake_addr" String
      :> Get '[JSON] AccountInfo

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
      :> Get  '[JSON] [Asset]

  , _accountsHistory
      ::  route
      :- Capture "stake_addr" String
      :> "history"
      :> QueryParam "epoch_no" EpochNo
      :> Pagination
      :> Get  '[JSON] [AccountHistory]

  , _accountsReward
      ::  route
      :- Capture "stake_addr" String
      :> "rewards"
      :> Pagination
      :> Get  '[JSON] [AccountReward]

  , _accountsUpdates
      ::  route
      :- Capture "stake_addr" String
      :> "updates"
      :> Pagination
      :> Get  '[JSON] [AccountUpdate]

  } deriving (Generic)
