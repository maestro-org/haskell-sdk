module Maestro.Client.Env
  (
    MaestroEnv(..)
  , mkMaestroEnv
  , MaestroNetwork(..)
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

maestroBaseUrl :: MaestroNetwork -> String
maestroBaseUrl Preprod = "https://preprod.gomaestro-api.org/v0"
maestroBaseUrl Mainnet = "https://mainnet.gomaestro-api.org/v0"

mkMaestroEnv :: MaestroToken -> MaestroNetwork -> IO MaestroEnv
mkMaestroEnv token nid = do
  clientEnv <- servantClientEnv $ maestroBaseUrl nid
  pure $ MaestroEnv { _maeClientEnv = clientEnv, _maeToken = token }

servantClientEnv :: String -> IO Servant.ClientEnv
servantClientEnv url = do
  baseUrl <- Servant.parseBaseUrl url
  manager <- newManager tlsManagerSettings
  pure $ Servant.mkClientEnv manager baseUrl
