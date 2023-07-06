module Maestro.Client.V0.Epochs
  ( getCurrentEpoch
  , getEpochInfo
  ) where

import           Maestro.API.V0         (_epochs)
import           Maestro.API.V0.Epochs
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

epochsClient :: MaestroEnv 'V0 -> EpochsAPI (AsClientT IO)
epochsClient = fromServant . _epochs . apiV0Client

-- | Get information about the current epoch.
getCurrentEpoch :: MaestroEnv 'V0 -> IO CurrentEpochInfo
getCurrentEpoch = _currentEpochInfo . epochsClient

-- | Get information about a specific epoch.
getEpochInfo :: MaestroEnv 'V0 -> EpochNo -> IO EpochInfo
getEpochInfo = _epochInfo . epochsClient
