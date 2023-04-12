module Maestro.API where

import           Data.Proxy           (Proxy (..))
import           Maestro.API.Accounts
import           Servant.API
import           Servant.API.Generic

data MaestroApi route  = Routes
  {
    _accounts :: route :- "accounts" :> ToServantApi AccountsAPI
  } deriving(Generic)

api :: Proxy (ToServantApi MaestroApi)
api = genericApi (Proxy :: Proxy MaestroApi)
