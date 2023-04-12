module Maestro.Client
  (
    apiClient
  ) where

import           Control.Exception      (throwIO)
import           Maestro.API
import           Maestro.Client.Env
import           Servant.Client
import           Servant.Client.Generic

apiClient :: MaestroEnv -> MaestroApi (AsClientT IO)
apiClient MaestroEnv{..} = genericClientHoist $ \x -> runClientM x _maeClientEnv >>= either throwIO pure