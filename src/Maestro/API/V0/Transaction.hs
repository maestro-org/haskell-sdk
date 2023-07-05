module Maestro.API.V0.Transaction where

import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import           Maestro.Types.V0
import           Servant.API
import           Servant.API.Generic

data TxAPI route = TxAPI
  { _submitTx ::
      route
        :- "submit"
        :> "tx"
        :> ReqBody' '[Required] '[CBORStream] BS.ByteString
        :> PostAccepted '[JSON] T.Text,
    _txCborApi ::
      route
        :- "transactions"
        :> Capture "tx_hash" (HashStringOf Tx)
        :> "cbor"
        :> Get '[JSON] TxCbor,
    _txAddressApi ::
      route
        :- "transactions"
        :> Capture "tx_hash" (HashStringOf Tx)
        :> "outputs"
        :> Capture "index" TxIndex
        :> "address"
        :> Get '[JSON] UtxoAddress,
    _txUtxo ::
      route
        :- "transactions"
        :> Capture "tx_hash" (HashStringOf Tx)
        :> "outputs"
        :> Capture "index" TxIndex
        :> "utxo"
        :> QueryParam "resolve_datums" Bool
        :> QueryParam "with_cbor" Bool
        :> Get '[JSON] Utxo
  }
  deriving (Generic)
