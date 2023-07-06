module Maestro.Client.V0.Address where

import           Data.Text              (Text)
import           Maestro.API.V0
import           Maestro.API.V0.Address
import           Maestro.Client.Env
import           Maestro.Client.V0.Core
import           Maestro.Types.V0
import           Servant.API.Generic
import           Servant.Client

addressClient :: MaestroEnv 'V0 -> AddressAPI (AsClientT IO)
addressClient = fromServant . _address . apiV0Client

-- |
-- Returns list of utxos for multiple addresses
utxosAtMultiAddresses ::
  -- | The Maestro Environment
  MaestroEnv 'V0 ->
  -- | Query param to include the corresponding datums for datum hashes
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response
  Maybe Bool ->
  -- | The pagination attributes
  Page ->
  -- | List of Address in bech32 format to fetch utxo from
  [Text] ->
  IO [Utxo]
utxosAtMultiAddresses = _addressesUtxos . addressClient

-- |
-- Returns list of utxo for specific address
utxosAtAddress ::
  MaestroEnv 'V0 ->
  -- | The Address in bech32 format
  Text ->
  -- | Query param to include the corresponding datums for datum hashes
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response
  Maybe Bool ->
  -- | The pagination attributes
  Page ->
  IO [Utxo]
utxosAtAddress = _addressUtxo . addressClient

-- |
-- Returns list of utxo ref for address
getRefsAtAddress ::
  MaestroEnv 'V0 ->
  -- | The Address in bech32 format
  Text ->
  -- | The pagination attributes
  Page ->
  IO [UtxoRef]
getRefsAtAddress = _addressUtxoRefs . addressClient

-- |
-- Get the transaction count for an address
getTxCountForAddress ::
  MaestroEnv 'V0 ->
  -- | The Address in bech32 format
  Text ->
  IO [AddressTxCount]
getTxCountForAddress = _addressTransactionCount . addressClient
