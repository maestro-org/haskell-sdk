module Maestro.Types.Common where

import qualified Data.Aeson     as Aeson
import           Data.Char      (toLower)
import           Data.Text      (Text)
import           Data.Word      (Word64)
import           Deriving.Aeson
import           GHC.Natural    (Natural)

-- | An epoch, i.e. the number of the epoch.
newtype EpochNo = EpochNo {unEpochNo :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral, ToJSON, FromJSON)

-- | Number of slot in Epoch
newtype EpochSlot = EpochSlot {unEpochSlot :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

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

data DatumType =  Inline | Hash
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[LowerFirst]] DatumType

data Datum = MaestroDatum
  { _datumBytes :: !(Maybe Text)
  , _datumHash  :: !Text
  , _datumJson  :: !(Maybe Aeson.Value)
  , _datumType  :: !DatumType
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_datum", LowerFirst]] Datum


data ScriptType =  Native | Plutusv1 | Plutusv2
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[LowerFirst]] ScriptType

data ReferenceScript = ReferenceScript
  { _refScriptBytes :: !(Maybe Text)
  , _refScriptHash  :: !Text
  , _refScriptJson  :: !(Maybe Aeson.Value)
  , _refScriptType  :: !ScriptType
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_refScript", LowerFirst]] ReferenceScript

data MaestroAsset =  MaestroAsset
  { _maestroAssetQuantity :: !Integer
  , _maestroAssetUnit     :: !(Maybe String)
  , _maestroAssetName     :: !(Maybe String)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maestroAsset", CamelToSnake]] MaestroAsset

-- | Transaction output
data Utxo = Utxo
  { _utxoAddress         :: !Text
  , _utxoAssets          :: ![MaestroAsset]
  , _utxoDatum           :: !(Maybe Datum)
  , _utxoIndex           :: !Natural
  , _utxoReferenceScript :: !(Maybe ReferenceScript)
  , _utxoTxHash          :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxo", CamelToSnake]] Utxo

newtype TxCbor = TxCbor {_txCbor :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_tx", LowerFirst]] TxCbor

newtype TxAddress = TxAddress {_txAddress :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_tx", LowerFirst]] TxAddress

data MaestroOrder = ASC | DESC

instance Show MaestroOrder where
  show ASC  = "asc"
  show DESC = "desc"

-- | Will lower the first character for your type.
data LowerFirst
instance StringModifier LowerFirst where
  getStringModifier ""       = ""
  getStringModifier (c : cs) = toLower c : cs
