{-# LANGUAGE LambdaCase #-}

module Maestro.Client.V1.Core
  ( apiV1Client
  , module Maestro.Client.V1.Core.Pagination
  ) where
import           Control.Exception                 (throwIO)
import           Control.Retry                     (retrying, limitRetriesByCumulativeDelay, exponentialBackoff)
import           Maestro.API.V1
import           Maestro.Client.Env
import           Maestro.Client.Error              (fromServantClientError, MaestroError (..))
import           Maestro.Client.V1.Core.Pagination
import           Servant.API.Generic               (fromServant)
import           Servant.Client
import           Servant.Client.Generic
import Control.Monad ((>=>))


apiV1ClientAuth :: MaestroEnv 'V1 -> MaestroApiV1Auth (AsClientT IO)
apiV1ClientAuth MaestroEnv{..} =
  genericClientHoist $
    do
      let handler = case (_maeBaseDelay , _maeMaxDelay) of
            (Just bDelay, Just mDelay) ->
              \x ->
                retrying
                  (limitRetriesByCumulativeDelay mDelay$ exponentialBackoff bDelay)
                  (\_retryStatus -> \case
                      Right _ -> pure False
                      Left clientErr -> case fromServantClientError clientErr of
                        MaestroUsageLimitReached -> pure True
                        _ -> pure False
                  )
                  (\_ -> runClientM x _maeClientEnv)
            _ -> \x -> runClientM x _maeClientEnv
      handler >=> either (throwIO . fromServantClientError) pure

apiV1Client :: MaestroEnv 'V1 -> MaestroApiV1 (AsClientT IO)
apiV1Client mEnv@MaestroEnv {..} = fromServant $ _apiV1 (apiV1ClientAuth mEnv) _maeToken
