-- | Common (shared) types between different versions of Maestro-API.

module Maestro.Types.Common
  ( Tx,
    TxOutCbor,
    DatumHash,
    Address,
    TxIndex (..),
    PolicyId (..),
    TokenName (..),
    EpochNo (..),
    EpochSize (..),
    AbsoluteSlot (..),
    SlotNo (..),
    BlockHeight (..),
    BlockHash (..),
    TxHash (..),
    Bech32StringOf (..),
    HexStringOf (..),
    HashStringOf (..),
    DatumOptionType (..),
    DatumOption (..),
    ScriptType (..),
    Script (..),
    Order (..),
    CBORStream,
    LowerFirst,
    LowerAll,
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

-- | Phantom datatype to be used with `HexStringOf` to represent hex encoded CBOR bytes of transaction output.
data TxOutCbor

-- | Phantom datatype to be used with `HexStringOf` to represent hex encoded datum hash.
data DatumHash

-- | Phantom datatype to be used with, say `Bech32StringOf` to represent Bech32 representation of an address.
data Address

-- | Index of UTxO in a transaction outputs.
newtype TxIndex = TxIndex Natural
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, Real, Integral, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

-- | Hex encoded minting policy ID (for non-ada native asset).
newtype PolicyId = PolicyId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

-- | Hex encoded token name (for non-ada native asset).
newtype TokenName = TokenName Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

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
newtype TxHash = TxHash Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

-- | Type to label the string is question is a @Bech32@ representation of the given type @a@.
newtype Bech32StringOf a = Bech32StringOf Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

-- | Type to label the string is question is a hexadecimal representation of the given type @a@.
newtype HexStringOf a = HexStringOf Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

-- | Type to label the string is question is a hash string of the given type @a@, like hash of the transaction body.
newtype HashStringOf a = HashStringOf Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

-- | Datum in output is either inlined or not.
data DatumOptionType = Inline | Hash
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] DatumOptionType

-- | Description of datum in an output. If datum is successfully resolved for (when mentioning to resolve for it by giving @resolve_datums@ flag in query parameters) then fields like @_datumOptionBytes@ would have non `Nothing` value even if UTxO just had hash of datum.
data DatumOption = DatumOption
  { _datumOptionBytes :: !(Maybe Text),
  -- ^ Hex encoded datum CBOR bytes.
    _datumOptionHash  :: !Text,
  -- ^ Hash of the datum.
    _datumOptionJson  :: !(Maybe Aeson.Value),
  -- ^ JSON representation of the datum.
    _datumOptionType  :: !DatumOptionType
  -- ^ See `DatumOptionType`.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_datumOption", LowerFirst]] DatumOption

-- | Type of script.
data ScriptType = Native | PlutusV1 | PlutusV2
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[LowerAll]] ScriptType

-- | Type to represent script in an UTxO.
data Script = Script
  { _scriptBytes :: !(Maybe Text),
  -- ^ Script bytes (`Nothing` if `Native` script).
    _scriptHash  :: !Text,
  -- ^ Hash of script.
    _scriptJson  :: !(Maybe Aeson.Value),
  -- ^ JSON representation of script (`Nothing` if not `Native` script).
    _scriptType  :: !ScriptType
  -- ^ See `ScriptType`.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_script", LowerFirst]] Script

-- Datatype to represent for /"order"/ query parameter in some of the API requests.
data Order = Ascending | Descending

-- Don't change @Show@ instance blindly, as `ToHttpApiData` instance is making use of it.
instance Show Order where
  show Ascending  = "asc"
  show Descending = "desc"

instance ToHttpApiData Order where
  toQueryParam order = T.pack $ show order

instance Default Order where
  def = Ascending

-- | Content-Type to represent transaction when submitting for it.
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
