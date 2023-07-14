-- | Module to query for /"addresses"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/addresses).

module Maestro.Client.V1.Addresses (
    utxosAtMultiAddresses,
    getRefsAtAddress,
  ) where

import           Maestro.API.V1
import           Maestro.API.V1.Addresses
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.Common     (Address, Bech32StringOf)
import           Maestro.Types.V1         (PaginatedOutputReferenceObject,
                                           PaginatedUtxoWithSlot)
import           Servant.API.Generic
import           Servant.Client

addressClient :: MaestroEnv 'V1 -> AddressesAPI (AsClientT IO)
addressClient = fromServant . _addresses . apiV1Client

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
utxosAtMultiAddresses = _addressesUtxos . addressClient

-- | UTxO IDs for all the unspent transaction outputs at an address.
getRefsAtAddress ::
  MaestroEnv 'V1 ->
  -- | The Address in Bech32 format.
  Bech32StringOf Address ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedOutputReferenceObject
getRefsAtAddress = _addressUtxoRefs . addressClient
