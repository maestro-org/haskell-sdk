-- | Module to define types for /\"Transactions\"/ category endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/transactions).

module Maestro.Types.V1.Transactions
  ( OutputReference (..)
  , UtxoWithBytes (..)
  , v1UtxoToV0
  , PaginatedUtxo (..)
  , TxDetails (..)
  , TimestampedTxDetails (..)
  ) where

import           Data.Aeson              (ToJSON (..), Value (..))
import           Data.Coerce             (coerce)
import           Data.Text               (Text)
import           Data.Time               (NominalDiffTime)
import           Data.Word               (Word64)
import           Deriving.Aeson
import           GHC.Natural             (Natural)
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

-- | Complete transaction Details when queried by its hash.
data TxDetails = TxDetails
  { _txDetailsTxHash :: !TxHash
  -- ^ Transaction hash (identifier)
  , _txDetailsBlockHash :: !BlockHash
  -- ^ Hash of the block which includes the transaction.
  , _txDetailsBlockAbsoluteSlot :: !Word64
  -- ^ Absolute slot of the block which includes the transaction
  , _txDetailsBlockHeight :: !BlockHeight
  -- ^ Block height (number) of the block which includes the transaction
  , _txDetailsBlockTimestamp :: !NominalDiffTime
  -- ^ UNIX timestamp of the block which includes the transaction
  , _txDetailsBlockTxIndex :: !Natural
  -- ^ The transaction's position within the block which includes it
  , _txDetailsDeposit :: !Word64
  -- ^ The amount of lovelace used for deposits (negative if being returned)
  , _txDetailsFee :: !Word64
  -- ^ The fee specified in the transaction
  , _txDetailsSize :: !Word64
  -- ^ Size of the transaction in bytes
  , _txDetailsScriptsSuccessful :: !Bool
  -- ^ False if any executed Plutus scripts failed (aka phase-two validity),
  --   meaning collateral was processed.
  , _txDetailsInvalidBefore :: !(Maybe SlotNo)
  -- ^ The slot before which the transaction would not be accepted onto the chain
  , _txDetailsInvalidHereafter :: !(Maybe SlotNo)
  -- ^ The slot from which the transaction would not be accepted onto the chain
  , _txDetailsMetadata :: !(Maybe Value)
  -- ^ Transaction metadata JSON
  , _txDetailsAdditionalSigners :: ![Text]
  -- ^ Additional required signers
  , _txDetailsOutputs :: ![UtxoWithBytes]
  -- ^ Transaction outputs
  , _txDetailsInputs :: ![UtxoWithBytes]
  -- ^ Transaction inputs
  , _txDetailsReferenceInputs :: ![UtxoWithBytes]
  -- ^ Reference inputs
  , _txDetailsCollateralInputs :: ![UtxoWithBytes]
  -- ^ Collateral inputs, to be taken if Plutus scripts are not successful
  , _txDetailsCollateralReturn :: !(Maybe UtxoWithBytes)
  -- ^ Transaction output
  , _txDetailsMint :: ![Asset]
  -- ^ Native assets minted or burned by the transaction
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_txDetails", CamelToSnake]] TxDetails

-- | Timestamped `TxDetails` response.
data TimestampedTxDetails = TimestampedTxDetails
  { _timestampedTxDetailsData        :: !TxDetails
  -- ^ See `TxDetails`.
  , _timestampedTxDetailsLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_timestampedTxDetails", CamelToSnake]] TimestampedTxDetails

instance IsTimestamped TimestampedTxDetails where
  type TimestampedData TimestampedTxDetails = TxDetails
  getTimestampedData = _timestampedTxDetailsData
  getTimestamp = _timestampedTxDetailsLastUpdated
