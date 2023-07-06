module Maestro.Client.V0.TxManager
  ( submitAndMonitorTx
  )
where

import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import           Maestro.API.V0           (_txManager)
import           Maestro.API.V0.TxManager
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Servant.API.Generic
import           Servant.Client

txClient :: MaestroEnv 'V0 -> TxManagerAPI (AsClientT IO)
txClient = fromServant . _txManager . apiV0Client

-- |
-- Submit a signed and serialized transaction to the network.
-- A transaction submited with this endpoint will be monitored by Maestro.
submitAndMonitorTx ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | CBOR encoded Transaction
  BS.ByteString ->
  IO Text
submitAndMonitorTx = _monitorTxSubmit . txClient
