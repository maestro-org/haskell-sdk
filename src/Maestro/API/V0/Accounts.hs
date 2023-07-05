module Maestro.API.V0.Accounts where

import           Data.Text                         (Text)
import           Maestro.Client.V0.Core.Pagination
import           Maestro.Types.V0
import           Servant.API
import           Servant.API.Generic

data AccountsAPI route = AccountsAPI
  {
    _account
      :: route
      :- Capture "stake_addr" Text
      :> Get '[JSON] AccountInfo

  , _accountAddresses
      ::  route
      :- Capture "stake_addr" Text
      :> "addresses"
      :> Pagination
      :> Get '[JSON] [Text]

  , _accountAssets
      ::  route
      :- Capture "stake_addr" Text
      :> "assets"
      :> Pagination
      :> Get  '[JSON] [Asset]

  , _accountsHistory
      ::  route
      :- Capture "stake_addr" Text
      :> "history"
      :> QueryParam "epoch_no" EpochNo
      :> Pagination
      :> Get  '[JSON] [AccountHistory]

  , _accountsReward
      ::  route
      :- Capture "stake_addr" Text
      :> "rewards"
      :> Pagination
      :> Get  '[JSON] [AccountReward]

  , _accountsUpdates
      ::  route
      :- Capture "stake_addr" Text
      :> "updates"
      :> Pagination
      :> Get  '[JSON] [AccountUpdate]

  } deriving (Generic)
