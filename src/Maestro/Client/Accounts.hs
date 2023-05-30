module Maestro.Client.Accounts where

import           Data.Text            (Text)
import           Maestro.API
import           Maestro.API.Accounts
import           Maestro.Client.Core
import           Maestro.Client.Env
import           Maestro.Types
import           Servant.API.Generic
import           Servant.Client

accountsClient :: MaestroEnv -> AccountsAPI (AsClientT IO)
accountsClient = fromServant . _accounts . apiClient

getAccount :: MaestroEnv -> Text -> IO AccountInfo
getAccount = _account . accountsClient

listAccountAddresses :: MaestroEnv -> Text -> Page -> IO [Text]
listAccountAddresses = _accountAddresses . accountsClient

listAccountAssets :: MaestroEnv -> Text -> Page -> IO [Asset]
listAccountAssets = _accountAssets . accountsClient

listAccountHistory :: MaestroEnv -> Text -> Maybe EpochNo -> Page -> IO [AccountHistory]
listAccountHistory = _accountsHistory . accountsClient

listAccountRewards :: MaestroEnv -> Text -> Page -> IO [AccountReward]
listAccountRewards = _accountsReward . accountsClient

listAccountUpdates :: MaestroEnv -> Text -> Page -> IO [AccountUpdate]
listAccountUpdates = _accountsUpdates . accountsClient
