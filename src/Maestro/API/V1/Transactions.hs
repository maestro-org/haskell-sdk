module Maestro.API.V1.Transactions where

import           Maestro.Client.V1.Core.Pagination
import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data TransactionsAPI route = TransactionsAPI
  { txOutputs
      :: route
      :- "outputs"
      :> QueryParam "resolve_datums" Bool
      :> QueryParam "with_cbor" Bool
      :> Pagination
      :> ReqBody '[JSON] [OutputReference]
      :> Post '[JSON] PaginatedUtxo
  , txInfo
      :: route
      :- Capture "tx_hash" TxHash
      :> Get '[JSON] TimestampedTxDetails
  }
  deriving (Generic)
