module Maestro.Client.Env
  ( MaestroEnv (..)
  , MaestroNetwork (..)
  , MaestroApiVersion (..)
  , mkMaestroEnv
  , defaultBackoff
  ) where

import           Data.Functor            ((<&>))
import           Data.Text               (Text)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Servant.Client          as Servant

type MaestroToken = Text

data MaestroApiVersion = V0 | V1

instance Show MaestroApiVersion where
  show V0 = "v0"
  show V1 = "v1"

data SingMaestroApiVersion (v :: MaestroApiVersion) where
  SingV0 :: SingMaestroApiVersion 'V0
  SingV1 :: SingMaestroApiVersion 'V1

fromSingMaestroApiVersion :: SingMaestroApiVersion v -> MaestroApiVersion
fromSingMaestroApiVersion SingV0 = V0
fromSingMaestroApiVersion SingV1 = V1

class SingMaestroApiVersionI (v :: MaestroApiVersion)
  where singMaestroApiVersion :: SingMaestroApiVersion v

instance SingMaestroApiVersionI 'V0 where singMaestroApiVersion = SingV0
instance SingMaestroApiVersionI 'V1 where singMaestroApiVersion = SingV1

data MaestroEnv (v :: MaestroApiVersion) = MaestroEnv
  { maeClientEnv :: !Servant.ClientEnv
  , maeToken     :: !MaestroToken
  , maeBaseDelay :: !(Maybe Int)
  -- ^ Base delay in microseconds to use with jitter backoff.
  -- https://hackage.haskell.org/package/retry-0.9.3.1/docs/Control-Retry.html#v:exponentialBackoff
  , maeMaxDelay  :: !(Maybe Int)
  -- ^ Maximum waiting time in microseconds.
  -- https://hackage.haskell.org/package/retry-0.9.3.1/docs/Control-Retry.html#v:limitRetriesByCumulativeDelay
  }

data MaestroNetwork = Mainnet | Preprod | Preview

maestroBaseUrl :: MaestroNetwork -> MaestroApiVersion -> String
maestroBaseUrl Preview v = "https://preview.gomaestro-api.org/" <> show v
maestroBaseUrl Preprod v = "https://preprod.gomaestro-api.org/" <> show v
maestroBaseUrl Mainnet v = "https://mainnet.gomaestro-api.org/" <> show v

mkMaestroEnv
  :: forall (apiVersion :: MaestroApiVersion).
  ( SingMaestroApiVersionI apiVersion
  ) =>
  MaestroToken ->
  MaestroNetwork ->
  Maybe (Int, Int) ->
  IO (MaestroEnv apiVersion)
mkMaestroEnv token nid mbDelays = do
  clientEnv <- servantClientEnv $ maestroBaseUrl nid (fromSingMaestroApiVersion $ singMaestroApiVersion @apiVersion)
  pure $ MaestroEnv
    { maeClientEnv = clientEnv
    , maeToken = token
    , maeBaseDelay = mbDelays <&> fst
    , maeMaxDelay = mbDelays <&> snd
    }

servantClientEnv :: String -> IO Servant.ClientEnv
servantClientEnv url = do
  baseUrl <- Servant.parseBaseUrl url
  manager <- newManager tlsManagerSettings
  pure $ Servant.mkClientEnv manager baseUrl

-- | Base delay & maximum waiting in microseconds corresponding to `maeBaseDelay`, `maeMaxDelay` respectively. See description of `maeBaseDelay` & `maeMaxDelay` for more information.
defaultBackoff :: Maybe (Int, Int)
defaultBackoff = Just (1_000_000, 10_000_000)
