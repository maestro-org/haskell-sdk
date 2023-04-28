module Maestro.API.General where

import           Maestro.Types.General
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
    } deriving (Generic)
