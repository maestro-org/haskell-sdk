-- | Module to define types for /\"Transactions\"/ category endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/transactions).

module Maestro.Types.V1.Transactions
  ( OutputReference (..)
  , Utxo (..)
  , v1UtxoToV0
  , PaginatedUtxo (..)
  ) where

import           Data.Aeson              (ToJSON (..), Value (..))
import           Data.Coerce             (coerce)
import qualified Data.Text               as T (pack)
import           Deriving.Aeson
import           Maestro.Types.Common
import qualified Maestro.Types.V0        as V0 (Utxo (..))
import           Maestro.Types.V1.Common
import           Servant.API             (ToHttpApiData (..))

-- | An UTxO output reference.
data OutputReference = OutputReference !TxHash !TxIndex
  deriving stock (Show, Eq, Ord)

instance ToHttpApiData OutputReference where
  toQueryParam (OutputReference txHash txIndex) = coerce txHash <> T.pack (show txIndex)

instance ToJSON OutputReference where
  toJSON outputReference = String $ toQueryParam outputReference

-- | Transaction output.
data Utxo = Utxo
  { _utxoAddress         :: !(Bech32StringOf Address),
  -- ^ UTxO's address.
    _utxoAssets          :: ![Asset],
  -- ^ UTxO's assets.
    _utxoDatum           :: !(Maybe DatumOption),
  -- ^ UTxO's datum.
    _utxoIndex           :: !TxIndex,
  -- ^ UTxO's transaction index.
    _utxoReferenceScript :: !(Maybe Script),
  -- ^ UTxO's script.
    _utxoTxHash          :: !TxHash,
  -- ^ UTxO's transaction hash.
    _utxoTxoutCbor       :: !(Maybe (HexStringOf TxOutCbor))
  -- ^ Hex encoded transaction output CBOR bytes.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxo", CamelToSnake]] Utxo

-- | Convert @V1@ API version UTxO type into corresponding @V0@ type.
v1UtxoToV0 :: Utxo -> V0.Utxo
v1UtxoToV0 Utxo {..} = V0.Utxo {
    V0._utxoAddress = _utxoAddress
  , V0._utxoAssets = map v1AssetToV0 _utxoAssets
  , V0._utxoDatum = _utxoDatum
  , V0._utxoIndex = coerce _utxoIndex
  , V0._utxoReferenceScript = _utxoReferenceScript
  , V0._utxoTxHash = coerce _utxoTxHash
  , V0._utxoTxoutCbor = _utxoTxoutCbor
  }

-- | A paginated response of transaction outputs.
data PaginatedUtxo = PaginatedUtxo
  { _paginatedUtxoData        :: ![Utxo],
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

instance HasCursor PaginatedUtxo where
  type CursorData PaginatedUtxo = [Utxo]
  getNextCursor utxos = _paginatedUtxoNextCursor utxos
  getCursorData utxos = _paginatedUtxoData utxos
