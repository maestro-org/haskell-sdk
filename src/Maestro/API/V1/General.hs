module Maestro.API.V1.General where

import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data GeneralAPI route =
  GeneralAPI
    {
      systemStart
        :: route
        :- "system-start"
        :> Get '[JSON] TimestampedSystemStart
    , eraHistory
        :: route
        :- "era-summaries"
        :> Get '[JSON] TimestampedEraSummaries
    , protocolParams
        :: route
        :- "protocol-parameters"
        :> Get '[JSON] TimestampedProtocolParameters
    , chainTip
        :: route
        :- "chain-tip"
        :> Get '[JSON] TimestampedChainTip
    } deriving Generic
