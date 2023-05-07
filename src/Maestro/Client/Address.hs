module Maestro.Client.Address where

import Data.Text (Text)
import Maestro.API
import Maestro.API.Address
import Maestro.Client
import Maestro.Client.Env
import Maestro.Types.Address
import Maestro.Util.Pagination (Page)
import Servant.API.Generic
import Servant.Client

addressClient :: MaestroEnv -> AddressAPI (AsClientT IO)
addressClient = fromServant . _address . apiClient

-- |
-- Returns list of utxos for multiple addresses
utxosForMultiAddresses ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Query param to include the corresponding datums for datum hashes
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response
  Maybe Bool ->
  -- | The pagination attributes
  Page ->
  -- | List of Address in bech32 format to fetch utxo from
  [Text] ->
  IO [AddressUtxo]
utxosForMultiAddresses = _addressesUtxos . addressClient

-- |
-- Returns list of utxo for specific address
utxosForAddress ::
  MaestroEnv ->
  -- | The Address in bech32 format
  Text ->
  -- | Query param to include the corresponding datums for datum hashes
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response
  Maybe Bool ->
  -- | The pagination attributes
  Page ->
  IO [AddressUtxo]
utxosForAddress = _addressUtxo . addressClient

-- |
-- Returns list of utxo ref for address
getUtxoRef ::
  MaestroEnv ->
  -- | The Address in bech32 format
  Text ->
  -- | The pagination attributes
  Page ->
  IO [AddressUtxoRef]
getUtxoRef = _addressUtxoRefs . addressClient

-- |
-- Get the transaction count for an address
getTxCount ::
  MaestroEnv ->
  -- | The Address in bech32 format
  Text ->
  IO [AddressTxCount]
getTxCount = _addressTransactionCount . addressClient
