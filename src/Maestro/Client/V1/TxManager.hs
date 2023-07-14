module Maestro.Client.V1.TxManager
  ( submitAndMonitorTx
  , turboSubmitAndMonitorTx
  )
where

import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import           Maestro.API.V1           (_txManager)
import           Maestro.API.V1.TxManager
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Servant.API.Generic
import           Servant.Client

txClient :: MaestroEnv 'V1 -> TxManagerAPI (AsClientT IO)
txClient = fromServant . _txManager . apiV1Client

-- | Submit a signed and serialized transaction to the network. A transaction submited with this endpoint will be monitored by Maestro.
submitAndMonitorTx ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | CBOR encoded Transaction.
  BS.ByteString ->
  IO Text
submitAndMonitorTx = _monitoredTxSubmit . txClient

-- | Submit a signed and serialized transaction to the network. A transaction submited with this endpoint will be [turbo submitted & monitored by Maestro](https://docs.gomaestro.org/docs/API%20reference/Transaction%20Manager/tx-manager-turbo-submit).
turboSubmitAndMonitorTx ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | CBOR encoded Transaction.
  BS.ByteString ->
  IO Text
turboSubmitAndMonitorTx = _monitoredTurboTxSubmit . txClient
