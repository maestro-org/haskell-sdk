module Maestro.API.V0.General where

import           Maestro.Types.V0
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
        :> Get '[JSON] [EraSummary]
    , _protocolParams
        :: route
        :- "protocol-params"
        :> Get '[JSON] ProtocolParameters
    , _chainTip
        :: route
        :- "chain-tip"
        :> Get '[JSON] ChainTip
    } deriving Generic
