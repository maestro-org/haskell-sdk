module Maestro.API.V1.Address where

import           Data.Text                         (Text)
import           Maestro.Client.V1.Core.Pagination
import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data AddressAPI route = AddressAPI
  {
    _addressesUtxos
      :: route
      :- "utxos"
      :> QueryParam "resolve_datums"  Bool
      :> QueryParam "with_cbor"  Bool
      :> Pagination
      :> ReqBody '[JSON] [Text]
      :> Post '[JSON] Utxos

  } deriving (Generic)
