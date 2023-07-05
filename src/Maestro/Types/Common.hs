module Maestro.Types.Common
  ( Tx,
    TxIndex (..),
    PolicyId (..),
    AssetId (..),
    CBORStream,
    EpochNo (..),
    EpochSize (..),
    AbsoluteSlot (..),
    SlotNo (..),
    BlockHeight (..),
    BlockHash (..),
    TxHash (..),
    Bech32StringOf (..),
    HexStringOf,
    HashStringOf (..),
    DatumOptionType (..),
    DatumOption (..),
    ScriptType (..),
    Script (..),
    Asset (..),
    TxCbor (..),
    UtxoAddress (..),
    Order (..),
    LowerFirst,
  )
where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char            (toLower)
import           Data.Default.Class
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Word            (Word64)
import           Deriving.Aeson
import           GHC.Natural          (Natural)
import           Servant.API

-- | Phantom datatype to be used with constructors like `HashStringOf`.
data Tx

-- | Index of UTxO in a transaction outputs.
newtype TxIndex = TxIndex Natural
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, Real, Integral, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

-- | Minting policy ID.
newtype PolicyId = PolicyId Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

-- | Concatenation of hex encoded policy ID and hex encoded asset name.
newtype AssetId = AssetId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

-- | An epoch, i.e. the number of the epoch.
newtype EpochNo = EpochNo {unEpochNo :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral, ToJSON, FromJSON)

instance ToHttpApiData EpochNo where
  toQueryParam = T.pack . show . unEpochNo

-- | Length of an epoch, i.e., number of slots in it.
newtype EpochSize = EpochSize {unEpochSize :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral, ToJSON, FromJSON)

-- | Absolute Slot Number
newtype AbsoluteSlot = AbsoluteSlot {unAbsoluteSlot :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

-- | The 0-based index for the Ourboros time slot.
newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Bounded, Enum, Real, Integral, FromJSON, ToJSON)

-- | Block Height
newtype BlockHeight = BlockHeight {unBlockHeight :: Natural}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Real, Integral, FromJSON, ToJSON)

-- | Hash of the block.
newtype BlockHash = BlockHash {unBlockHash :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)

-- | Hash of the Transaction.
newtype TxHash = TxHash {unTxHash :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON)

newtype Bech32StringOf a = Bech32StringOf Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

type HexStringOf a = Text

newtype HashStringOf a = HashStringOf Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

data DatumOptionType = Inline | Hash
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] DatumOptionType

data DatumOption = DatumOption
  { _datumOptionBytes :: !(Maybe Text),
    _datumOptionHash  :: !Text,
    _datumOptionJson  :: !(Maybe Aeson.Value),
    _datumOptionType  :: !DatumOptionType
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_datumOption", LowerFirst]] DatumOption

data ScriptType = Native | PlutusV1 | PlutusV2
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[LowerAll]] ScriptType

data Script = Script
  { _scriptBytes :: !(Maybe Text),
    _scriptHash  :: !Text,
    _scriptJson  :: !(Maybe Aeson.Value),
    _scriptType  :: !ScriptType
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_script", LowerFirst]] Script

data Asset = Asset
  { _assetQuantity :: !Integer
  , _assetUnit     :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_asset", CamelToSnake]] Asset

newtype TxCbor = TxCbor {_txCbor :: Text}
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_tx", LowerFirst]] TxCbor

newtype UtxoAddress = UtxoAddress {_utxoAddressAddress :: Text}
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoAddress", LowerFirst]] UtxoAddress

data Order = Ascending | Descending

instance ToHttpApiData Order where
  toQueryParam Ascending  = "asc"
  toQueryParam Descending = "desc"

instance Default Order where
  def = Ascending

instance Show Order where
  show Ascending  = "asc"
  show Descending = "desc"

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

-- | Will lower the first character for your type.
data LowerFirst

instance StringModifier LowerFirst where
  getStringModifier ""       = ""
  getStringModifier (c : cs) = toLower c : cs

-- | Will lower all characters for your type.
data LowerAll

instance StringModifier LowerAll where
  getStringModifier cs = toLower <$> cs
