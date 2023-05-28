module Maestro.API.Datum where

import           Data.Text           (Text)
import           Maestro.Types.Datum
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
