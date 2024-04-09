-- | Module to query for /"addresses"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/addresses).

module Maestro.Client.V1.Addresses (
    utxosAtAddress,
    utxosAtMultiAddresses,
    getRefsAtAddress,
    utxosByPaymentCredential,
    utxosByMultiPaymentCredentials,
    txsByAddress,
    txsByPaymentCredential,
  ) where

import           Maestro.API.V1
import           Maestro.API.V1.Addresses
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.Common       (Address, Bech32StringOf, Order,
                                             SlotNo)
import           Maestro.Types.V1           (NonAdaNativeToken,
                                             PaginatedOutputReferenceObject,
                                             PaginatedPaymentCredentialTransaction,
                                             PaginatedUtxoWithSlot,
                                             PaymentCredentialAddress)
import           Maestro.Types.V1.Addresses (PaginatedAddressTransaction)
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
  -- | Query Param to return for only those UTxOs which contain this given asset.
  Maybe NonAdaNativeToken ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedUtxoWithSlot
utxosByPaymentCredential = paymentCredentialUtxos . addressClient

-- | Returns list of utxos for multiple payment credentials.
utxosByMultiPaymentCredentials ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | Query param to include the corresponding datums for datum hashes.
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response.
  Maybe Bool ->
  -- | The pagination attributes.
  Cursor ->
  -- | List of payment credential in bech32 format to fetch utxo from.
  [Bech32StringOf PaymentCredentialAddress] ->
  IO PaginatedUtxoWithSlot
utxosByMultiPaymentCredentials = paymentCredentialsUtxos . addressClient

-- | Returns transactions in which the specified address spent or received funds.
--
-- Specifically, the transactions where: the address controlled at least one of the transaction inputs and/or receives one of the outputs AND the transaction is phase-2 valid, OR, the address controlled at least one of the collateral inputs and/or receives the collateral return output AND the transaction is phase-2 invalid. [Read more](https://docs.cardano.org/plutus/collateral-mechanism/).
txsByAddress ::
  MaestroEnv 'V1 ->
  -- | Address in bech32 format.
  Bech32StringOf Address ->
  -- | The order in which the results are sorted (by point in chain).
  Maybe Order ->
  -- | Return only transactions minted on or after a specific slot.
  Maybe SlotNo ->
  -- | Return only transactions minted on or before a specific slot.
  Maybe SlotNo ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedAddressTransaction
txsByAddress = addressTxs . addressClient

-- | Returns transactions in which the specified payment credential spent or received funds, or was a required signer.
--
-- Specifically, "spent or received funds" meaning: the payment credential was used in an address which controlled at least one of the transaction inputs and/or receives one of the outputs AND the transaction is phase-2 valid, OR, the address controlled at least one of the collateral inputs and/or receives the collateral return output AND the transaction is phase-2 invalid. [Read more](https://docs.cardano.org/plutus/collateral-mechanism/).
txsByPaymentCredential ::
  MaestroEnv 'V1 ->
  -- | Payment credential in bech32 format.
  Bech32StringOf PaymentCredentialAddress ->
  -- | The order in which the results are sorted (by point in chain).
  Maybe Order ->
  -- | Return only transactions minted on or after a specific slot.
  Maybe SlotNo ->
  -- | Return only transactions minted on or before a specific slot.
  Maybe SlotNo ->
  -- | The pagination attributes.
  Cursor ->
  IO PaginatedPaymentCredentialTransaction
txsByPaymentCredential = paymentCredentialTxs . addressClient
