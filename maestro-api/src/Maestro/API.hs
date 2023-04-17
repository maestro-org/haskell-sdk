module Maestro.API where

import           Data.Proxy           (Proxy (..))
import           Maestro.API.Accounts
import           Maestro.API.Address
import           Maestro.API.Assets
import           Servant.API
import           Servant.API.Generic

data MaestroApi route  = Routes
  {
    _accounts :: route :- "accounts" :> ToServantApi AccountsAPI
  , _address  :: route :- "addresses" :> ToServantApi AddressAPI
  , _assets   :: route :- "assets" :> ToServantApi AssetsAPI
  } deriving(Generic)

api :: Proxy (ToServantApi MaestroApi)
api = genericApi (Proxy :: Proxy MaestroApi)
