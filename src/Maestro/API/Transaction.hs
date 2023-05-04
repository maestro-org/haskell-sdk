module Maestro.API.Transaction where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Maestro.Types.Common
import           Servant.API
import           Servant.API.Generic  (Generic)


data CBORStream

instance Accept CBORStream where
  contentType _ = "application/cbor"

instance MimeRender CBORStream BS.ByteString where
    mimeRender _ = LBS.fromStrict

instance MimeRender CBORStream LBS.ByteString where
    mimeRender _ = id

instance MimeUnrender CBORStream BS.ByteString where
    mimeUnrender _ = Right . LBS.toStrict

instance MimeUnrender CBORStream LBS.ByteString where
    mimeUnrender _ = Right


data TxAPI route = TxAPI
  {
    _monitorTxSubmit
      :: route
      :- ReqBody' '[Required] '[CBORStream] BS.ByteString
      :> Post '[JSON] T.Text

  , _submitTx
      :: route
      :-  "submit"
      :>  "tx"
      :> ReqBody' '[Required] '[CBORStream] BS.ByteString
      :> Post '[JSON] T.Text

  , _txCborApi
    ::  route
    :- Capture "tx_hash"  T.Text
    :> "cbor"
    :> Get '[JSON] TxCbor

  , _txAddressApi
    ::  route
    :- Capture "tx_hash"  T.Text
    :> "outputs"
    :> Capture "index"  Int
    :> "address"
    :> Get '[JSON] TxAddress

  , _txUtxo
    ::  route
    :- Capture "tx_hash"  T.Text
    :> "outputs"
    :> Capture "index"  Int
    :> "utxo"
    :> QueryParam "resolve_datums"  Bool
    :> QueryParam "with_cbor"  Bool
    :> Get '[JSON] Utxo


  } deriving Generic
