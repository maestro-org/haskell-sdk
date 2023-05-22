module Maestro.Client.Epochs
  ( getCurrentEpoch
  , getEpochInfo
  ) where

import           Maestro.API         (_epochs)
import           Maestro.API.Epochs
import           Maestro.Client
import           Maestro.Client.Env
import           Maestro.Types
import           Servant.API.Generic
import           Servant.Client

epochsClient :: MaestroEnv -> EpochsAPI (AsClientT IO)
epochsClient = fromServant . _epochs . apiClient

getCurrentEpoch :: MaestroEnv -> IO CurrentEpochInfo
getCurrentEpoch = _currentEpochInfo . epochsClient

getEpochInfo :: MaestroEnv -> EpochNo -> IO EpochInfo
getEpochInfo = _epochInfo . epochsClient
