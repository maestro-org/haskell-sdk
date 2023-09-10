module Maestro.API.V1.Accounts where

import           Maestro.Client.V1.Core.Pagination
import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data AccountsAPI route = AccountsAPI
  { accountInfo
      :: route
      :- Capture "stake_addr" (Bech32StringOf RewardAddress)
      :> Get '[JSON] TimestampedAccountInfo

  , accountAddresses
      :: route
      :- Capture "stake_addr" (Bech32StringOf RewardAddress)
      :> "addresses"
      :> Pagination
      :> Get '[JSON] PaginatedAddress

  , accountAssets
      :: route
      :- Capture "stake_addr" (Bech32StringOf RewardAddress)
      :> "assets"
      :> QueryParam "policy" PolicyId
      :> Pagination
      :> Get '[JSON] PaginatedAsset

  , accountHistory
      :: route
      :- Capture "stake_addr" (Bech32StringOf RewardAddress)
      :> "history"
      :> QueryParam "epoch_no" EpochNo
      :> Pagination
      :> Get '[JSON] PaginatedAccountHistory

  , accountRewards
      :: route
      :- Capture "stake_addr" (Bech32StringOf RewardAddress)
      :> "rewards"
      :> Pagination
      :> Get '[JSON] PaginatedAccountReward

  , accountUpdates
      ::  route
      :- Capture "stake_addr" (Bech32StringOf RewardAddress)
      :> "updates"
      :> Pagination
      :> Get '[JSON] PaginatedAccountUpdate
  } deriving (Generic)
