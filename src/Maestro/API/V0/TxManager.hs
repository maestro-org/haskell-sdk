module Maestro.API.V0.TxManager where

import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import           Maestro.Types.V0
import           Servant.API
import           Servant.API.Generic

newtype TxManagerAPI route = TxManagerAPI
  { _monitorTxSubmit ::
      route
        :- ReqBody' '[Required] '[CBORStream] BS.ByteString
        :> PostAccepted '[JSON] T.Text
  }
  deriving (Generic)
