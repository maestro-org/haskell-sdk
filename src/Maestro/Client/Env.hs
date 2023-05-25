module Maestro.Client.Env
  (
    MaestroEnv(..)
  , mkMaestroEnv
  , MaestroNetwork(..)
  ) where

import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           Network.HTTP.Client     (managerModifyRequest, requestHeaders)
import           Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)

import qualified Servant.Client          as Servant

type MaestroToken = Text

newtype MaestroEnv = MaestroEnv
  { _maeClientEnv :: Servant.ClientEnv
  }

data MaestroNetwork = Mainnet | Preprod

maestroBaseUrl :: MaestroNetwork -> String
maestroBaseUrl Preprod = "https://preprod.gomaestro-api.org/v0"
maestroBaseUrl Mainnet = "https://mainnet.gomaestro-api.org/v0"

mkMaestroEnv :: MaestroToken -> MaestroNetwork -> IO MaestroEnv
mkMaestroEnv token nid = do
  clientEnv <- servantClientEnv token $ maestroBaseUrl nid
  pure $ MaestroEnv {_maeClientEnv = clientEnv}

servantClientEnv :: MaestroToken -> String -> IO Servant.ClientEnv
servantClientEnv token url = do
  baseUrl <- Servant.parseBaseUrl url
  manager <- newTlsManagerWith $ tlsManagerSettings {managerModifyRequest = addTokenHeader}

  pure $ Servant.mkClientEnv manager baseUrl

  where
    addTokenHeader req  = pure $ req {requestHeaders = [("api-key", encodeUtf8 token)]}
