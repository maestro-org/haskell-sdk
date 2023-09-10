-- | Module to query for /"general"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/general).

module Maestro.Client.V1.General
  ( getChainTip
  , getSystemStart
  , getEraHistory
  , getProtocolParameters
  ) where

import           Maestro.API.V1         (general)
import           Maestro.API.V1.General
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.V1
import           Servant.API.Generic
import           Servant.Client

generalClient :: MaestroEnv 'V1 -> GeneralAPI (AsClientT IO)
generalClient = fromServant . general . apiV1Client

-- | Get details about the latest block of the network.
getChainTip :: MaestroEnv 'V1 -> IO TimestampedChainTip
getChainTip = chainTip . generalClient

-- | Get network start time since genesis.
getSystemStart :: MaestroEnv 'V1 -> IO TimestampedSystemStart
getSystemStart = systemStart . generalClient

-- | Get network era history.
getEraHistory :: MaestroEnv 'V1 -> IO TimestampedEraSummaries
getEraHistory = eraHistory . generalClient

-- | Get protocol parameters for the latest epoch.
getProtocolParameters :: MaestroEnv 'V1 -> IO TimestampedProtocolParameters
getProtocolParameters = protocolParams . generalClient
