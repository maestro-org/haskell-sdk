module Maestro.API.V1.General where

import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data GeneralAPI route =
  GeneralAPI
    {
      _systemStart
        :: route
        :- "system-start"
        :> Get '[JSON] TimestampedSystemStart
    , _eraHistory
        :: route
        :- "era-history"
        :> Get '[JSON] TimestampedEraSummaries
    , _protocolParams
        :: route
        :- "protocol-params"
        :> Get '[JSON] TimestampedProtocolParameters
    , _chainTip
        :: route
        :- "chain-tip"
        :> Get '[JSON] TimestampedChainTip
    } deriving Generic
