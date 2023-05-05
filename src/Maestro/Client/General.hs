module Maestro.Client.General
  ( getChainTip
  , getSystemStart
  , getEraHistory
  , getProtocolParameters
  ) where

import           Maestro.API           (_general)
import           Maestro.API.General
import           Maestro.Client
import           Maestro.Client.Env
import           Maestro.Types.General
import           Servant.API.Generic
import           Servant.Client

generalClient :: MaestroEnv -> GeneralAPI (AsClientT IO)
generalClient = fromServant . _general . apiClient

-- | Get details about the latest block of the network.
getChainTip :: MaestroEnv -> IO ChainTip
getChainTip = _chainTip . generalClient

-- | Get network start time since genesis.
getSystemStart :: MaestroEnv -> IO SystemStart
getSystemStart = _systemStart . generalClient

-- | Get network era history.
getEraHistory :: MaestroEnv -> IO [EraSummary]
getEraHistory = _eraHistory . generalClient

-- | Get protocol parameters for the latest epoch.
getProtocolParameters :: MaestroEnv -> IO ProtocolParameters
getProtocolParameters = _protocolParams . generalClient
