module Maestro.Client.V1.Core
  ( apiV1Client
  , module Maestro.Client.V1.Core.Pagination
  ) where

import           Control.Exception                 (throwIO)
import           Maestro.API.V1
import           Maestro.Client.Env
import           Maestro.Client.Error              (fromServantClientError)
import           Maestro.Client.V1.Core.Pagination
import           Servant.API.Generic               (fromServant)
import           Servant.Client
import           Servant.Client.Generic

apiV1ClientAuth :: MaestroEnv 'V1 -> MaestroApiV1Auth (AsClientT IO)
apiV1ClientAuth MaestroEnv{..} = genericClientHoist $ \x -> runClientM x _maeClientEnv >>= either (throwIO . fromServantClientError) pure

apiV1Client :: MaestroEnv 'V1 -> MaestroApiV1 (AsClientT IO)
apiV1Client mEnv@MaestroEnv {..} = fromServant $ _apiV1 (apiV1ClientAuth mEnv) _maeToken
