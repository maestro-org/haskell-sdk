module Maestro.API.V0 where

import           Data.Text                  (Text)
import           Maestro.API.V0.Accounts
import           Maestro.API.V0.Address
import           Maestro.API.V0.Assets
import           Maestro.API.V0.Datum
import           Maestro.API.V0.Epochs
import           Maestro.API.V0.General
import           Maestro.API.V0.Pool
import           Maestro.API.V0.Scripts
import           Maestro.API.V0.Transaction
import           Maestro.API.V0.TxManager
import           Servant.API
import           Servant.API.Generic

data MaestroApiV0 route  = MaestroApiV0
  { _accounts  :: route :- "accounts" :> ToServantApi AccountsAPI
  , _address   :: route :- "addresses" :> ToServantApi AddressAPI
  , _assets    :: route :- "assets" :> ToServantApi AssetsAPI
  , _general   :: route :- ToServantApi GeneralAPI
  , _pools     :: route :- "pools" :> ToServantApi PoolAPI
  , _tx        :: route :- ToServantApi TxAPI
  , _epochs    :: route :- "epochs" :> ToServantApi EpochsAPI
  , _datum     :: route :- "datum" :> ToServantApi DatumAPI
  , _scripts   :: route :- "scripts" :> ToServantApi ScriptsAPI
  , _txManager :: route :- "txmanager" :> ToServantApi TxManagerAPI
  } deriving Generic

newtype MaestroApiV0Auth route = MaestroApiV0Auth
  { _apiV0 :: route :- Header' '[Required] "api-key" Text :> ToServantApi MaestroApiV0 }
  deriving Generic
