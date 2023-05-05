module Maestro.Client.Accounts where

import           Maestro.API
import           Maestro.API.Accounts
import           Maestro.Client
import           Maestro.Client.Env
import           Maestro.Types
import           Maestro.Util.Pagination (Page)
import           Servant.API.Generic
import           Servant.Client

accountsClient :: MaestroEnv -> AccountsAPI (AsClientT IO)
accountsClient = fromServant . _accounts . apiClient

getAccount :: MaestroEnv -> String -> IO AccountsInfo
getAccount = _account . accountsClient

listAccountAddresses :: MaestroEnv -> String -> Page -> IO [String]
listAccountAddresses = _accountAddresses . accountsClient

listAccountAssets :: MaestroEnv -> String -> Page -> IO [AccountsAssets]
listAccountAssets = _accountAssets . accountsClient

listAccountHistory :: MaestroEnv  -> String -> (Maybe EpochNo) -> Page -> IO [AccountsHistory]
listAccountHistory = _accountsHistory . accountsClient

listAccountRewards  :: MaestroEnv -> String -> Page -> IO [AccountsRewards]
listAccountRewards = _accountsReward . accountsClient

listAccountUpdates :: MaestroEnv  -> String -> Page -> IO [AccountsUpdates]
listAccountUpdates = _accountsUpdates . accountsClient
