module Maestro.API.V1 where

import           Data.Text              (Text)
import           Maestro.API.V1.Address
import           Maestro.API.V1.General
import           Servant.API
import           Servant.API.Generic

data MaestroApiV1 route  = MaestroApiV1
  { _general :: route :- ToServantApi GeneralAPI
  , _address :: route :- "addresses" :> ToServantApi AddressAPI
  } deriving Generic

newtype MaestroApiV1Auth route = MaestroApiV1Auth
  { _apiV1 :: route :- Header' '[Required] "api-key" Text :> ToServantApi MaestroApiV1 }
  deriving Generic
