-- | Module to query for /"accounts"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/accounts).

module Maestro.Client.V1.Accounts (
  accountInfo,
  accountAddresses,
  accountAssets,
  accountHistory,
  accountRewards,
  accountUpdates
) where

import           Maestro.API.V1
import qualified Maestro.API.V1.Accounts as Mapi
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.V1
import           Servant.API.Generic
import           Servant.Client

accountsClient :: MaestroEnv 'V1 -> Mapi.AccountsAPI (AsClientT IO)
accountsClient = fromServant . accounts . apiV1Client

-- | Returns various information regarding a stake account.
accountInfo ::
  MaestroEnv 'V1 ->
  -- | Bech32 encoded reward/stake address (@stake1...@).
  Bech32StringOf RewardAddress ->
  IO TimestampedAccountInfo
accountInfo = Mapi.accountInfo . accountsClient

-- | Returns a list of addresses seen on-chain which use the specified stake key.
accountAddresses ::
  MaestroEnv 'V1 ->
  -- | Bech32 encoded reward/stake address (@stake1...@).
  Bech32StringOf RewardAddress ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedAddress
accountAddresses = Mapi.accountAddresses . accountsClient

-- | Returns a list of native assets which are owned by addresses with the specified stake key.
accountAssets ::
  MaestroEnv 'V1 ->
  -- | Bech32 encoded reward/stake address (@stake1...@).
  Bech32StringOf RewardAddress ->
  -- | Filter results to only show assets of the specified policy.
  Maybe PolicyId ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedAsset
accountAssets = Mapi.accountAssets . accountsClient

-- | Returns per-epoch history for the specified stake key.
accountHistory ::
  MaestroEnv 'V1 ->
  -- | Bech32 encoded reward/stake address (@stake1...@).
  Bech32StringOf RewardAddress ->
  -- | Fetch result for only a specific epoch.
  Maybe EpochNo ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedAccountHistory
accountHistory = Mapi.accountHistory . accountsClient

-- | Returns a list of staking-related rewards for the specified stake key (pool @member@ or @leader@ rewards, deposit @refund@).
accountRewards ::
  MaestroEnv 'V1 ->
  -- | Bech32 encoded reward/stake address (@stake1...@).
  Bech32StringOf RewardAddress ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedAccountReward
accountRewards = Mapi.accountRewards . accountsClient

-- | Returns a list of updates relating to the specified stake key (@registration@, @deregistration@, @delegation@, @withdrawal@)
accountUpdates ::
  MaestroEnv 'V1 ->
  -- | Bech32 encoded reward/stake address (@stake1...@).
  Bech32StringOf RewardAddress ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedAccountUpdate
accountUpdates = Mapi.accountUpdates . accountsClient
