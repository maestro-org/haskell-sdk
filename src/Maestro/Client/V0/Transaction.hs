module Maestro.Client.V0.Transaction
  ( submitTx,
    txCbor,
    txAddress,
    txUtxo,
  )
where

import qualified Data.ByteString            as BS
import           Data.Text                  (Text)
import           Maestro.API.V0             (_tx)
import           Maestro.API.V0.Transaction
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

txClient :: MaestroEnv 'V0 -> TxAPI (AsClientT IO)
txClient = fromServant . _tx . apiV0Client

-- |
-- Submit a signed and serialized transaction to the network.
-- Interaction with this endpoint is identical to IOG's Cardano Submit API and will not be monitored by Maestro.
submitTx ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | CBOR encoded Transaction
  BS.ByteString ->
  IO Text
submitTx = _submitTx . txClient

-- |
-- Returns hex-encoded CBOR bytes of a transaction
txCbor ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | Hex Encoded Transaction Hash
  HashStringOf Tx ->
  -- | hex-encoded CBOR bytes of a transaction
  IO TxCbor
txCbor = _txCborApi . txClient

-- |
-- Returns the address specified in the given transaction output
txAddress ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | Hex Encoded Transaction Hash
  HashStringOf Tx ->
  -- | The Transaction Output Index
  TxIndex ->
  IO UtxoAddress
txAddress = _txAddressApi . txClient

-- |
-- Returns the specified transaction output of a transaction output reference
txUtxo ::
  -- | The Maestro Environment.
  MaestroEnv 'V0 ->
  -- | Hex encoded transaction hash
  HashStringOf Tx ->
  -- | The Transaction Output Index
  TxIndex ->
  -- | Try find and include the corresponding datums for datum hashes
  Maybe Bool ->
  -- | Include the CBOR encodings of the transaction outputs in the response
  Maybe Bool ->
  IO Utxo
txUtxo = _txUtxo . txClient
