module Maestro.Client.V0.General
  ( getChainTip
  , getSystemStart
  , getEraHistory
  , getProtocolParameters
  ) where

import           Maestro.API.V0           (_general)
import           Maestro.API.V0.General
import           Maestro.Client.Core
import           Maestro.Client.Env
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

generalClient :: MaestroEnv -> GeneralAPI (AsClientT IO)
generalClient = fromServant . _general . apiV0Client

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
