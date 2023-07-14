module Maestro.Client.V0.General
  ( getChainTip
  , getSystemStart
  , getEraHistory
  , getProtocolParameters
  ) where

import           Maestro.API.V0         (_general)
import           Maestro.API.V0.General
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

generalClient :: MaestroEnv 'V0 -> GeneralAPI (AsClientT IO)
generalClient = fromServant . _general . apiV0Client

-- | Get details about the latest block of the network.
getChainTip :: MaestroEnv 'V0 -> IO ChainTip
getChainTip = _chainTip . generalClient

-- | Get network start time since genesis.
getSystemStart :: MaestroEnv 'V0 -> IO SystemStart
getSystemStart = _systemStart . generalClient

-- | Get network era history.
getEraHistory :: MaestroEnv 'V0 -> IO [EraSummary]
getEraHistory = _eraHistory . generalClient

-- | Get protocol parameters for the latest epoch.
getProtocolParameters :: MaestroEnv 'V0 -> IO ProtocolParameters
getProtocolParameters = _protocolParams . generalClient
