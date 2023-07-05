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

data MaestroEnv = MaestroEnv
  { _maeClientEnv :: !Servant.ClientEnv
  , _maeToken     :: !MaestroToken
  }

data MaestroNetwork = Mainnet | Preprod

data MaestroApiVersion = V0 | V1

-- TODO : Move version check inside.
maestroBaseUrl :: MaestroNetwork -> MaestroApiVersion -> String
maestroBaseUrl Preprod V0 = "https://preprod.gomaestro-api.org/v0"
maestroBaseUrl Preprod V1 = "https://preprod.gomaestro-api.org/v1"
maestroBaseUrl Mainnet V0 = "https://mainnet.gomaestro-api.org/v0"
maestroBaseUrl Mainnet V1 = "https://mainnet.gomaestro-api.org/v1"

mkMaestroEnv :: MaestroToken -> MaestroNetwork -> MaestroApiVersion -> IO MaestroEnv
mkMaestroEnv token nid apiVersion = do
  clientEnv <- servantClientEnv $ maestroBaseUrl nid apiVersion
  pure $ MaestroEnv { _maeClientEnv = clientEnv, _maeToken = token }

servantClientEnv :: String -> IO Servant.ClientEnv
servantClientEnv url = do
  baseUrl <- Servant.parseBaseUrl url
  manager <- newManager tlsManagerSettings
  pure $ Servant.mkClientEnv manager baseUrl
