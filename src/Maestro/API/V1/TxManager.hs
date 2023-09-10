module Maestro.API.V1.TxManager where

import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data TxManagerAPI route = TxManagerAPI
  { monitoredTxSubmit ::
      route
        :- ReqBody' '[Required] '[CBORStream] BS.ByteString
        :> PostAccepted '[JSON] T.Text
  , monitoredTurboTxSubmit ::
      route
        :- ReqBody' '[Required] '[CBORStream] BS.ByteString
        :> PostAccepted '[JSON] T.Text
  }
  deriving (Generic)
