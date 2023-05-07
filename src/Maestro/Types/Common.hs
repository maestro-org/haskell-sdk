module Maestro.Types.Common
  ( Tx,
    TxIndex (..),
    PolicyId (..),
    AssetId (..),
    EpochNo (..),
    EpochSize (..),
    AbsoluteSlot (..),
    BlockHeight (..),
    BlockHash (..),
    TxHash (..),
    Bech32StringOf (..),
    HexStringOf,
    HashStringOf (..),
    DatumType (..),
    Datum (..),
    ScriptType (..),
    ReferenceScript (..),
    MaestroAsset (..),
    Utxo (..),
    TxCbor (..),
    TxAddress (..),
    Order (..),
    LowerFirst,
  )
where

import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Default.Class
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Deriving.Aeson
import GHC.Natural (Natural)
import Web.HttpApiData

-- | Phantom datatype to be used with constructors like `HashStringOf`.
data Tx

-- | Index of UTxO in a transaction outputs.
newtype TxIndex = TxIndex Natural
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

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

-- | Block Height
newtype BlockHeight = BlockHeight {unBlockHeight :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

-- | Hash of the block.
newtype BlockHash = BlockHash {unBlockHash :: String}
  deriving stock (Show, Eq, Generic)
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

data DatumType = Inline | Hash
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[LowerFirst]] DatumType

data Datum = Datum
  { _datumBytes :: !(Maybe Text),
    _datumHash :: !Text,
    _datumJson :: !(Maybe Aeson.Value),
    _datumType :: !DatumType
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_datum", LowerFirst]] Datum

data ScriptType = Native | Plutusv1 | Plutusv2
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[LowerFirst]] ScriptType

data ReferenceScript = ReferenceScript
  { _refScriptBytes :: !(Maybe Text),
    _refScriptHash :: !Text,
    _refScriptJson :: !(Maybe Aeson.Value),
    _refScriptType :: !ScriptType
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_refScript", LowerFirst]] ReferenceScript

data MaestroAsset = MaestroAsset
  { _maestroAssetQuantity :: !Integer,
    _maestroAssetUnit :: !(Maybe String),
    _maestroAssetName :: !(Maybe String)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_maestroAsset", CamelToSnake]] MaestroAsset

-- | Transaction output
data Utxo = Utxo
  { _utxoAddress :: !Text,
    _utxoAssets :: ![MaestroAsset],
    _utxoDatum :: !(Maybe Datum),
    _utxoIndex :: !Natural,
    _utxoReferenceScript :: !(Maybe ReferenceScript),
    _utxoTxHash :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxo", CamelToSnake]] Utxo

newtype TxCbor = TxCbor {_txCbor :: Text}
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_tx", LowerFirst]] TxCbor

newtype TxAddress = TxAddress {_txAddress :: Text}
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_tx", LowerFirst]] TxAddress

data Order = Ascending | Descending

instance ToHttpApiData Order where
  toQueryParam Ascending = "asc"
  toQueryParam Descending = "desc"

instance Default Order where
  def = Ascending

instance Show Order where
  show Ascending = "asc"
  show Descending = "desc"

-- | Will lower the first character for your type.
data LowerFirst

instance StringModifier LowerFirst where
  getStringModifier "" = ""
  getStringModifier (c : cs) = toLower c : cs
