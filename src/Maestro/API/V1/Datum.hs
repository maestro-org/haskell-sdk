module Maestro.API.V1.Datum where

import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

newtype DatumAPI route =
  DatumAPI
    {
      _datumByHash
        :: route
        :- Capture "datum_hash" (HexStringOf DatumHash)
        :> Get '[JSON] TimestampedDatum
    } deriving Generic
