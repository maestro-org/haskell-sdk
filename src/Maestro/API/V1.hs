module Maestro.API.V1 where

import           Data.Text              (Text)
import           Maestro.API.V0.General
import           Servant.API
import           Servant.API.Generic

data MaestroApiV1 route  = MaestroApiV1
  { _general   :: route :- ToServantApi GeneralAPI
  } deriving Generic

newtype MaestroApiV1Auth route = MaestroApiV1Auth
  { _apiV1 :: route :- Header' '[Required] "api-key" Text :> ToServantApi MaestroApiV1 }
  deriving Generic
