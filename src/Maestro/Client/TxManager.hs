module Maestro.Client.Transaction
  ( submitAndMonitorTx
  )
where

import qualified Data.ByteString       as BS
import           Data.Text             (Text)
import           Maestro.API           (_txManager)
import           Maestro.API.TxManager
import           Maestro.Client.Core
import           Maestro.Client.Env
import           Servant.API.Generic
import           Servant.Client

txClient :: MaestroEnv -> TxManagerAPI (AsClientT IO)
txClient = fromServant . _txManager . apiV0Client

-- |
-- Submit a signed and serialized transaction to the network.
-- A transaction submited with this endpoint will be monitored by Maestro.
submitAndMonitorTx ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | CBOR encoded Transaction
  BS.ByteString ->
  IO Text
submitAndMonitorTx = _monitorTxSubmit . txClient
