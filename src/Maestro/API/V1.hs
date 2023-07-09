module Maestro.API.V1 where

import           Data.Text                   (Text)
import           Maestro.API.V1.Addresses
import           Maestro.API.V1.Datum
import           Maestro.API.V1.General
import           Maestro.API.V1.Pools
import           Maestro.API.V1.Transactions
import           Maestro.API.V1.TxManager
import           Servant.API
import           Servant.API.Generic

data MaestroApiV1 route  = MaestroApiV1
  { _general      :: route :- ToServantApi GeneralAPI
  , _addresses    :: route :- "addresses" :> ToServantApi AddressesAPI
  , _datum        :: route :- "datum" :> ToServantApi DatumAPI
  , _pools        :: route :- "pools" :> ToServantApi PoolsAPI
  , _txManager    :: route :- "txmanager" :> ToServantApi TxManagerAPI
  , _transactions :: route :- ToServantApi TransactionsAPI
  } deriving Generic

newtype MaestroApiV1Auth route = MaestroApiV1Auth
  { _apiV1 :: route :- Header' '[Required] "api-key" Text :> ToServantApi MaestroApiV1 }
  deriving Generic
