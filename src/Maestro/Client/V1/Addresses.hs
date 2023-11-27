-- | Module to query for /"addresses"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/addresses).

module Maestro.Client.V1.Addresses (
    utxosAtAddress,
    utxosAtMultiAddresses,
    getRefsAtAddress,
    utxosByPaymentCredential,
  ) where

import           Maestro.API.V1
import           Maestro.API.V1.Addresses
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.Common     (Address, Bech32StringOf)
import           Maestro.Types.V1         (NonAdaNativeToken,
                                           PaginatedOutputReferenceObject,
                                           PaginatedUtxoWithSlot,
                                           PaymentCredentialAddress)
import           Servant.API.Generic
import           Servant.Client

addressClient :: MaestroEnv 'V1 -> AddressesAPI (AsClientT IO)
addressClient = fromServant . addresses . apiV1Client

-- | Returns list of utxos for multiple addresses.
utxosAtMultiAddresses ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | Query param to include the corresponding datums for datum hashes.
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response.
  Maybe Bool ->
  -- | The pagination attributes.
  Cursor ->
  -- | List of Address in bech32 format to fetch utxo from.
  [Bech32StringOf Address] ->
  IO PaginatedUtxoWithSlot
utxosAtMultiAddresses = addressesUtxos . addressClient

-- | Returns list of utxos for a given address.
utxosAtAddress ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | Address in bech32 format to fetch utxo from.
  Bech32StringOf Address ->
  -- | Query param to include the corresponding datums for datum hashes.
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response.
  Maybe Bool ->
  -- | Query Param to return for only those UTxOs which contain this given asset.
  Maybe NonAdaNativeToken ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedUtxoWithSlot
utxosAtAddress = addressUtxos . addressClient

-- | UTxO IDs for all the unspent transaction outputs at an address.
getRefsAtAddress ::
  MaestroEnv 'V1 ->
  -- | The Address in Bech32 format.
  Bech32StringOf Address ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedOutputReferenceObject
getRefsAtAddress = addressUtxoRefs . addressClient

-- | Query UTxOs by payment credential in bech32 format.
utxosByPaymentCredential ::
  MaestroEnv 'V1 ->
  -- | The Address in Bech32 format.
  Bech32StringOf PaymentCredentialAddress ->
  -- | Query param to include the corresponding datums for datum hashes.
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response.
  Maybe Bool ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedUtxoWithSlot
utxosByPaymentCredential = paymentCredentialUtxos . addressClient
