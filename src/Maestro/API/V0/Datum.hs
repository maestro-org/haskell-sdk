module Maestro.API.V0.Datum where

import           Data.Text           (Text)
import           Maestro.Types.V0
import           Servant.API
import           Servant.API.Generic

newtype DatumAPI route =
  DatumAPI
    {
      _datumByHash
        :: route
        :- Capture "datum_hash" Text
        :> Get '[JSON] Datum
    } deriving Generic
