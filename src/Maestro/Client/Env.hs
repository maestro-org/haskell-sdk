module Maestro.Client.Env
  (
    MaestroEnv (..)
  , MaestroNetwork (..)
  , MaestroApiVersion (..)
  , mkMaestroEnv
  ) where

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
  { _maeClientEnv :: !Servant.ClientEnv
  , _maeToken     :: !MaestroToken
  }

data MaestroNetwork = Mainnet | Preprod | Preview

maestroBaseUrl :: MaestroNetwork -> MaestroApiVersion -> String
maestroBaseUrl Preview v = "https://preview.gomaestro-api.org/" <> show v
maestroBaseUrl Preprod v = "https://preprod.gomaestro-api.org/" <> show v
maestroBaseUrl Mainnet v = "https://mainnet.gomaestro-api.org/" <> show v

mkMaestroEnv :: forall (apiVersion :: MaestroApiVersion). SingMaestroApiVersionI apiVersion => MaestroToken -> MaestroNetwork -> IO (MaestroEnv apiVersion)
mkMaestroEnv token nid = do
  clientEnv <- servantClientEnv $ maestroBaseUrl nid (fromSingMaestroApiVersion $ singMaestroApiVersion @apiVersion)
  pure $ MaestroEnv { _maeClientEnv = clientEnv, _maeToken = token }

servantClientEnv :: String -> IO Servant.ClientEnv
servantClientEnv url = do
  baseUrl <- Servant.parseBaseUrl url
  manager <- newManager tlsManagerSettings
  pure $ Servant.mkClientEnv manager baseUrl
