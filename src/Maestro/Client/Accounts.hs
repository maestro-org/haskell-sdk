module Maestro.Client.Accounts where

import           Maestro.API
import           Maestro.API.Accounts
import           Maestro.Client.Core
import           Maestro.Client.Env
import           Maestro.Types
import           Servant.API.Generic
import           Servant.Client

accountsClient :: MaestroEnv -> AccountsAPI (AsClientT IO)
accountsClient = fromServant . _accounts . apiClient

getAccount :: MaestroEnv -> String -> IO AccountInfo
getAccount = _account . accountsClient

listAccountAddresses :: MaestroEnv -> String -> Page -> IO [String]
listAccountAddresses = _accountAddresses . accountsClient

listAccountAssets :: MaestroEnv -> String -> Page -> IO [Asset]
listAccountAssets = _accountAssets . accountsClient

listAccountHistory :: MaestroEnv -> String -> Maybe EpochNo -> Page -> IO [AccountHistory]
listAccountHistory = _accountsHistory . accountsClient

listAccountRewards :: MaestroEnv -> String -> Page -> IO [AccountReward]
listAccountRewards = _accountsReward . accountsClient

listAccountUpdates :: MaestroEnv -> String -> Page -> IO [AccountUpdate]
listAccountUpdates = _accountsUpdates . accountsClient
