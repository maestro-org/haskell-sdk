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
        :> Get '[JSON] SystemStart
    , _eraHistory
        :: route
        :- "era-history"
        :> Get '[JSON] EraSummaries
    , _protocolParams
        :: route
        :- "protocol-params"
        :> Get '[JSON] ProtocolParameters
    , _chainTip
        :: route
        :- "chain-tip"
        :> Get '[JSON] ChainTip
    } deriving Generic
