module Maestro.API.V1.Addresses where

import           Maestro.Client.V1.Core.Pagination
import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data AddressesAPI route = AddressesAPI
  {
    _decodeAddress
      :: route
      :- Capture "address" (TaggedText AddressToDecode)
      :> "decode"
      :> Get '[JSON] AddressInfo

  , _addressesUtxos
      :: route
      :- "utxos"
      :> QueryParam "resolve_datums" Bool
      :> QueryParam "with_cbor" Bool
      :> Pagination
      :> ReqBody '[JSON] [Bech32StringOf Address]
      :> Post '[JSON] PaginatedUtxoWithSlot

  , _addressUtxoRefs
      :: route
      :- Capture "address" (Bech32StringOf Address)
      :> "utxo_refs"
      :> Pagination
      :> Get '[JSON] PaginatedOutputReferenceObject

  } deriving (Generic)
