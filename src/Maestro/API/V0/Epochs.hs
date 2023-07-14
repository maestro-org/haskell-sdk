module Maestro.API.V0.Epochs where

import           Maestro.Types.V0
import           Servant.API
import           Servant.API.Generic

data EpochsAPI route =
  EpochsAPI
    {
      _currentEpochInfo
        :: route
        :- "current"
        :> Get '[JSON] CurrentEpochInfo
    , _epochInfo
        :: route
        :- Capture "epoch_no" EpochNo
        :> "info"
        :> Get '[JSON] EpochInfo
    } deriving (Generic)
