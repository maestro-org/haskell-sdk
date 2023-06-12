module Maestro.API.TxManager where

import qualified Data.ByteString      as BS
import qualified Data.Text            as T
import           Maestro.Types.Common
import           Servant.API
import           Servant.API.Generic

newtype TxManagerAPI route = TxManagerAPI
  { _monitorTxSubmit ::
      route
        :- ReqBody' '[Required] '[CBORStream] BS.ByteString
        :> Post '[JSON] T.Text
  }
  deriving (Generic)
