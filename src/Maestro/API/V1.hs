module Maestro.API.V1 where

import           Data.Text                   (Text)
import           Maestro.API.V1.Accounts
import           Maestro.API.V1.Addresses
import           Maestro.API.V1.Blocks
import           Maestro.API.V1.Datum
import           Maestro.API.V1.DefiMarkets
import           Maestro.API.V1.General
import           Maestro.API.V1.Pools
import           Maestro.API.V1.Transactions
import           Maestro.API.V1.TxManager
import           Servant.API
import           Servant.API.Generic

data MaestroApiV1 route  = MaestroApiV1
  { general      :: route :- ToServantApi GeneralAPI
  , accounts     :: route :- "accounts" :> ToServantApi AccountsAPI
  , addresses    :: route :- "addresses" :> ToServantApi AddressesAPI
  , blocks       :: route :- "blocks" :> ToServantApi BlocksAPI
  , datums       :: route :- "datums" :> ToServantApi DatumAPI
  , defiMarkets  :: route :- "markets" :> "dexs" :> ToServantApi DefiMarketsAPI
  , pools        :: route :- "pools" :> ToServantApi PoolsAPI
  , txManager    :: route :- "txmanager" :> ToServantApi TxManagerAPI
  , transactions :: route :- "transactions" :> ToServantApi TransactionsAPI
  } deriving Generic

newtype MaestroApiV1Auth route = MaestroApiV1Auth
  { apiV1 :: route :- Header' '[Required] "api-key" Text :> ToServantApi MaestroApiV1 }
  deriving Generic
