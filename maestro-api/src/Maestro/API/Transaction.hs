module Maestro.API.Transaction where

import           Data.Text            (Text)
import           Maestro.Types.Common
import           Servant.API
import           Servant.API.Generic  (Generic)

data TxAPI route = TxAPI
  {
    _monitorTxSubmit
      :: route
      :- ReqBody' '[Required] '[]Text
      :> Post '[Text] Text

  , _submitTx
      :: route
      :-  "submit"
      :>  "tx"
      :> ReqBody' '[Required] '[]Text
      :> Post '[Text] Text

  , _txCbor
    ::  route
    :- Capture "tx_hash"  Text
    :> "cbor"
    :> Get '[JSON] TxCbor

  , _txAddress
    ::  route
    :- Capture "tx_hash"  Text
    :> "outputs"
    :> Capture "index"  Text
    :> "address"
    :> Get '[JSON] TxAddress

  , _txUtxo
    ::  route
    :- Capture "tx_hash"  Text
    :> "outputs"
    :> Capture "index"  Text
    :> "utxo"
    :> QueryParam "resolve_datums"  Bool
    :> QueryParam "with_cbor"  Bool
    :> Get '[JSON] Utxo


  } deriving Generic
