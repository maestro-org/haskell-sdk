module Maestro.API.V1.Addresses where

import           Maestro.Client.V1.Core.Pagination
import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data AddressesAPI route = AddressesAPI
  {
    decodeAddress
      :: route
      :- Capture "address" (TaggedText AddressToDecode)
      :> "decode"
      :> Get '[JSON] AddressInfo

  , addressUtxos
      :: route
      :- Capture "address" (Bech32StringOf Address)
      :> "utxos"
      :> QueryParam "resolve_datums" Bool
      :> QueryParam "with_cbor" Bool
      :> QueryParam "asset" NonAdaNativeToken
      :> Pagination
      :> Get '[JSON] PaginatedUtxoWithSlot

  , addressesUtxos
      :: route
      :- "utxos"
      :> QueryParam "resolve_datums" Bool
      :> QueryParam "with_cbor" Bool
      :> Pagination
      :> ReqBody '[JSON] [Bech32StringOf Address]
      :> Post '[JSON] PaginatedUtxoWithSlot

  , addressUtxoRefs
      :: route
      :- Capture "address" (Bech32StringOf Address)
      :> "utxo_refs"
      :> Pagination
      :> Get '[JSON] PaginatedOutputReferenceObject

  , paymentCredentialUtxos
      :: route
      :- "cred"
      :> Capture "credential" (Bech32StringOf PaymentCredentialAddress)
      :> "utxos"
      :> QueryParam "resolve_datums" Bool
      :> QueryParam "with_cbor" Bool
      -- TODO: Support for more query parameters.
      :> Pagination
      :> Get '[JSON] PaginatedUtxoWithSlot

  } deriving (Generic)
