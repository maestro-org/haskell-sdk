-- | Module to define types for /\"Transactions\"/ category endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/transactions).

module Maestro.Types.V1.Transactions
  ( OutputReference (..)
  , UtxoWithBytes (..)
  , v1UtxoToV0
  , PaginatedUtxo (..)
  ) where

import           Data.Aeson              (ToJSON (..), Value (..))
import           Data.Coerce             (coerce)
import           Deriving.Aeson
import           Maestro.Types.Common
import qualified Maestro.Types.V0        as V0 (Utxo (..))
import           Maestro.Types.V1.Common
import           Servant.API             (ToHttpApiData (..))

-- | An UTxO output reference.
data OutputReference = OutputReference !TxHash !TxIndex
  deriving stock (Show, Eq, Ord)

instance ToHttpApiData OutputReference where
  toQueryParam (OutputReference txHash txIndex) = toUrlPiece txHash <> "#" <> toUrlPiece txIndex

instance ToJSON OutputReference where
  toJSON outputReference = String $ toQueryParam outputReference

-- | Transaction output.
data UtxoWithBytes = UtxoWithBytes
  { _utxoWithBytesAddress         :: !(Bech32StringOf Address),
  -- ^ UTxO's address.
    _utxoWithBytesAssets          :: ![Asset],
  -- ^ UTxO's assets.
    _utxoWithBytesDatum           :: !(Maybe DatumOption),
  -- ^ UTxO's datum.
    _utxoWithBytesIndex           :: !TxIndex,
  -- ^ UTxO's transaction index.
    _utxoWithBytesReferenceScript :: !(Maybe Script),
  -- ^ UTxO's script.
    _utxoWithBytesTxHash          :: !TxHash,
  -- ^ UTxO's transaction hash.
    _utxoWithBytesTxoutCbor       :: !(Maybe (HexStringOf TxOutCbor))
  -- ^ Hex encoded transaction output CBOR bytes.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoWithBytes", CamelToSnake]] UtxoWithBytes

instance IsUtxo UtxoWithBytes where
  getAddress = _utxoWithBytesAddress
  getAssets = _utxoWithBytesAssets
  getDatum = _utxoWithBytesDatum
  getTxHash = _utxoWithBytesTxHash
  getIndex = _utxoWithBytesIndex
  getReferenceScript = _utxoWithBytesReferenceScript

-- | Convert @V1@ API version UTxO type into corresponding @V0@ type.
v1UtxoToV0 :: UtxoWithBytes -> V0.Utxo
v1UtxoToV0 UtxoWithBytes {..} = V0.Utxo {
    V0._utxoAddress = _utxoWithBytesAddress
  , V0._utxoAssets = map v1AssetToV0 _utxoWithBytesAssets
  , V0._utxoDatum = _utxoWithBytesDatum
  , V0._utxoIndex = coerce _utxoWithBytesIndex
  , V0._utxoReferenceScript = _utxoWithBytesReferenceScript
  , V0._utxoTxHash = coerce _utxoWithBytesTxHash
  , V0._utxoTxoutCbor = _utxoWithBytesTxoutCbor
  }

-- | A paginated response of transaction outputs.
data PaginatedUtxo = PaginatedUtxo
  { _paginatedUtxoData        :: ![UtxoWithBytes],
  -- ^ List of UTxOs.
    _paginatedUtxoLastUpdated :: !LastUpdated,
  -- ^ See `LastUpdated`.
    _paginatedUtxoNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_paginatedUtxo", CamelToSnake]] PaginatedUtxo

instance IsTimestamped PaginatedUtxo where
  type TimestampedData PaginatedUtxo = [UtxoWithBytes]
  getTimestampedData = _paginatedUtxoData
  getTimestamp = _paginatedUtxoLastUpdated

instance HasCursor PaginatedUtxo where
  getNextCursor = _paginatedUtxoNextCursor
