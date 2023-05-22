module Maestro.API.Epochs where

import           Maestro.Types.Common (EpochNo)
import           Maestro.Types.Epochs
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
