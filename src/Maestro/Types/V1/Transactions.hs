-- | Module to define types for /\"Transactions\"/ category endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/transactions).

module Maestro.Types.V1.Transactions
  ( OutputReference (..)
  , UtxoWithBytes (..)
  , PaginatedUtxo (..)
  , TxDetails (..)
  , TimestampedTxDetails (..)
  ) where

import           Data.Aeson              (ToJSON (..), Value (..))
import           Data.Text               (Text)
import           Data.Time               (NominalDiffTime)
import           Data.Word               (Word64)
import           Deriving.Aeson
import           GHC.Natural             (Natural)
import           Maestro.Types.Common
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
  { utxoWithBytesAddress         :: !(Bech32StringOf Address),
  -- ^ UTxO's address.
    utxoWithBytesAssets          :: ![Asset],
  -- ^ UTxO's assets.
    utxoWithBytesDatum           :: !(Maybe DatumOption),
  -- ^ UTxO's datum.
    utxoWithBytesIndex           :: !TxIndex,
  -- ^ UTxO's transaction index.
    utxoWithBytesReferenceScript :: !(Maybe Script),
  -- ^ UTxO's script.
    utxoWithBytesTxHash          :: !TxHash,
  -- ^ UTxO's transaction hash.
    utxoWithBytesTxoutCbor       :: !(Maybe (HexStringOf TxOutCbor))
  -- ^ Hex encoded transaction output CBOR bytes.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "utxoWithBytes", CamelToSnake]] UtxoWithBytes

instance IsUtxo UtxoWithBytes where
  getAddress = utxoWithBytesAddress
  getAssets = utxoWithBytesAssets
  getDatum = utxoWithBytesDatum
  getTxHash = utxoWithBytesTxHash
  getIndex = utxoWithBytesIndex
  getReferenceScript = utxoWithBytesReferenceScript

-- | A paginated response of transaction outputs.
data PaginatedUtxo = PaginatedUtxo
  { paginatedUtxoData        :: ![UtxoWithBytes],
  -- ^ List of UTxOs.
    paginatedUtxoLastUpdated :: !LastUpdated,
  -- ^ See `LastUpdated`.
    paginatedUtxoNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedUtxo", CamelToSnake]] PaginatedUtxo

instance IsTimestamped PaginatedUtxo where
  type TimestampedData PaginatedUtxo = [UtxoWithBytes]
  getTimestampedData = paginatedUtxoData
  getTimestamp = paginatedUtxoLastUpdated

instance HasCursor PaginatedUtxo where
  getNextCursor = paginatedUtxoNextCursor

-- | Complete transaction Details when queried by its hash.
data TxDetails = TxDetails
  { txDetailsTxHash :: !TxHash
  -- ^ Transaction hash (identifier)
  , txDetailsBlockHash :: !BlockHash
  -- ^ Hash of the block which includes the transaction.
  , txDetailsBlockAbsoluteSlot :: !Word64
  -- ^ Absolute slot of the block which includes the transaction
  , txDetailsBlockHeight :: !BlockHeight
  -- ^ Block height (number) of the block which includes the transaction
  , txDetailsBlockTimestamp :: !NominalDiffTime
  -- ^ UNIX timestamp of the block which includes the transaction
  , txDetailsBlockTxIndex :: !Natural
  -- ^ The transaction's position within the block which includes it
  , txDetailsDeposit :: !Word64
  -- ^ The amount of lovelace used for deposits (negative if being returned)
  , txDetailsFee :: !Word64
  -- ^ The fee specified in the transaction
  , txDetailsSize :: !Word64
  -- ^ Size of the transaction in bytes
  , txDetailsScriptsSuccessful :: !Bool
  -- ^ False if any executed Plutus scripts failed (aka phase-two validity),
  --   meaning collateral was processed.
  , txDetailsInvalidBefore :: !(Maybe SlotNo)
  -- ^ The slot before which the transaction would not be accepted onto the chain
  , txDetailsInvalidHereafter :: !(Maybe SlotNo)
  -- ^ The slot from which the transaction would not be accepted onto the chain
  , txDetailsMetadata :: !(Maybe Value)
  -- ^ Transaction metadata JSON
  , txDetailsAdditionalSigners :: ![Text]
  -- ^ Additional required signers
  , txDetailsOutputs :: ![UtxoWithBytes]
  -- ^ Transaction outputs
  , txDetailsInputs :: ![UtxoWithBytes]
  -- ^ Transaction inputs
  , txDetailsReferenceInputs :: ![UtxoWithBytes]
  -- ^ Reference inputs
  , txDetailsCollateralInputs :: ![UtxoWithBytes]
  -- ^ Collateral inputs, to be taken if Plutus scripts are not successful
  , txDetailsCollateralReturn :: !(Maybe UtxoWithBytes)
  -- ^ Transaction output
  , txDetailsMint :: ![Asset]
  -- ^ Native assets minted or burned by the transaction
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "txDetails", CamelToSnake]] TxDetails

-- | Timestamped `TxDetails` response.
data TimestampedTxDetails = TimestampedTxDetails
  { timestampedTxDetailsData        :: !TxDetails
  -- ^ See `TxDetails`.
  , timestampedTxDetailsLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedTxDetails", CamelToSnake]] TimestampedTxDetails

instance IsTimestamped TimestampedTxDetails where
  type TimestampedData TimestampedTxDetails = TxDetails
  getTimestampedData = timestampedTxDetailsData
  getTimestamp = timestampedTxDetailsLastUpdated
