module Maestro.Client.V0.Accounts where

import           Data.Text               (Text)
import           Maestro.API.V0
import           Maestro.API.V0.Accounts
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

accountsClient :: MaestroEnv 'V0 -> AccountsAPI (AsClientT IO)
accountsClient = fromServant . _accounts . apiV0Client

getAccount :: MaestroEnv 'V0 -> Text -> IO AccountInfo
getAccount = _account . accountsClient

listAccountAddresses :: MaestroEnv 'V0 -> Text -> Page -> IO [Text]
listAccountAddresses = _accountAddresses . accountsClient

listAccountAssets :: MaestroEnv 'V0 -> Text -> Page -> IO [Asset]
listAccountAssets = _accountAssets . accountsClient

listAccountHistory :: MaestroEnv 'V0 -> Text -> Maybe EpochNo -> Page -> IO [AccountHistory]
listAccountHistory = _accountsHistory . accountsClient

listAccountRewards :: MaestroEnv 'V0 -> Text -> Page -> IO [AccountReward]
listAccountRewards = _accountsReward . accountsClient

listAccountUpdates :: MaestroEnv 'V0 -> Text -> Page -> IO [AccountUpdate]
listAccountUpdates = _accountsUpdates . accountsClient
