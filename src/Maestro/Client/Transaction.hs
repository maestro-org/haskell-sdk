module Maestro.Client.Transaction
  ( submitAndMonitorTx,
    submitTx,
    txCbor,
    txAddress,
    txUtxo,
  )
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import GHC.Natural (Natural)
import Maestro.API (_tx)
import Maestro.API.Transaction
import Maestro.Client
import Maestro.Client.Env
import Maestro.Types.Common
import Servant.API.Generic
import Servant.Client

type CborEncodedByte = BS.ByteString

type TxIndex = Natural

type ResolveDatum = Bool

type WithCbor = Bool

--
txClient :: MaestroEnv -> TxAPI (AsClientT IO)
txClient = fromServant . _tx . apiClient

-- |
-- Submit a signed and serialized transaction to the network.
-- A transaction submited with this endpoint will be monitored by Maestro.
submitAndMonitorTx ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | CBOR encoded Transaction
  CborEncodedByte ->
  IO Text
submitAndMonitorTx = _monitorTxSubmit . txClient

-- |
-- Submit a signed and serialized transaction to the network.
-- Interaction with this endpoint is identical to IOG's Cardano Submit API and will not be monitored by Maestro.
submitTx ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | CBOR encoded Transaction
  CborEncodedByte ->
  IO Text
submitTx = _submitTx . txClient

-- |
-- Returns hex-encoded CBOR bytes of a transaction
txCbor ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Hex Encoded Transaction Hash
  HashStringOf Tx ->
  -- | hex-encoded CBOR bytes of a transaction
  IO TxCbor
txCbor = _txCborApi . txClient

-- |
-- Returns the address specified in the given transaction output
txAddress ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Hex Encoded Transaction Hash
  HashStringOf Tx ->
  -- | The Transaction Output Index
  TxIndex ->
  IO TxAddress
txAddress = _txAddressApi . txClient

-- |
-- Returns the specified transaction output of a transaction output reference
txUtxo ::
  -- | The MaestroEnv
  MaestroEnv ->
  -- | Hex encoded transaction hash
  HashStringOf Tx ->
  -- | The Transaction Output Index
  TxIndex ->
  -- | Try find and include the corresponding datums for datum hashes
  Maybe ResolveDatum ->
  -- | Include the CBOR encodings of the transaction outputs in the response
  Maybe WithCbor ->
  IO Utxo
txUtxo = _txUtxo . txClient
