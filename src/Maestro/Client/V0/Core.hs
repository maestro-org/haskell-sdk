module Maestro.Client.V0.Core
  ( apiV0Client
  , module Maestro.Client.V0.Core.Pagination
  ) where

import           Control.Exception                 (throwIO)
import           Maestro.API.V0
import           Maestro.Client.Env
import           Maestro.Client.Error              (fromServantClientError)
import           Maestro.Client.V0.Core.Pagination
import           Servant.API.Generic               (fromServant)
import           Servant.Client
import           Servant.Client.Generic


apiV0ClientAuth :: MaestroEnv 'V0 -> MaestroApiV0Auth (AsClientT IO)
apiV0ClientAuth MaestroEnv {..} = genericClientHoist $ \x -> runClientM x _maeClientEnv >>= either (throwIO . fromServantClientError) pure

apiV0Client :: MaestroEnv 'V0 -> MaestroApiV0 (AsClientT IO)
apiV0Client mEnv@MaestroEnv {..} = fromServant $ _apiV0 (apiV0ClientAuth mEnv) _maeToken
