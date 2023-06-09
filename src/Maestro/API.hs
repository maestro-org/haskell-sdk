module Maestro.API where

import           Data.Text               (Text)
import           Maestro.API.Accounts
import           Maestro.API.Address
import           Maestro.API.Assets
import           Maestro.API.Datum
import           Maestro.API.Epochs
import           Maestro.API.General
import           Maestro.API.Pool
import           Maestro.API.Scripts
import           Maestro.API.Transaction
import           Servant.API
import           Servant.API.Generic

data MaestroApiV0 route  = MaestroApiV0
  { _accounts :: route :- "accounts" :> ToServantApi AccountsAPI
  , _address  :: route :- "addresses" :> ToServantApi AddressAPI
  , _assets   :: route :- "assets" :> ToServantApi AssetsAPI
  , _general  :: route :- ToServantApi GeneralAPI
  , _pools    :: route :- "pools" :> ToServantApi PoolAPI
  , _tx       :: route :- ToServantApi TxAPI
  , _epochs   :: route :- "epochs" :> ToServantApi EpochsAPI
  , _datum    :: route :- "datum" :> ToServantApi DatumAPI
  , _scripts  :: route :- "scripts" :> ToServantApi ScriptsAPI
  } deriving Generic

newtype MaestroApiV0Auth route = MaestroApiV0Auth
  { _apiV0 :: route :- Header' '[Required] "api-key" Text :> ToServantApi MaestroApiV0 }
  deriving Generic
