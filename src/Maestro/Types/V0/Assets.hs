module Maestro.Types.V0.Assets where

import qualified Data.Aeson              as Aeson
import           Data.Text               (Text)
import           Data.Time.Clock.POSIX   (POSIXTime)
import           Deriving.Aeson
import           GHC.Natural             (Natural)
import           Maestro.Types.V0.Common (BlockHeight, EpochNo, LowerFirst)

data TokenRegistryMetadata = TokenRegistryMetadata
  { _tokenRegMetDecimals    :: !Integer
  , _tokenRegMetDescription :: !Text
  , _tokenRegMetLogo        :: !Text
  , _tokenRegMetName        :: !Text
  , _tokenRegMetTicker      :: !Text
  , _tokenRegMetUrl         :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_tokenRegMet", CamelToSnake]] TokenRegistryMetadata


data AssetStandards = AssetStandards
  { _assetStandardsCip25Metadata :: !(Maybe Aeson.Value)
  , _assetStandardsCip68Metadata :: !(Maybe Aeson.Value)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetStandards", CamelToSnake]] AssetStandards


data AssetInfo = AssetInfo
  { _assetInfoAssetName             :: !Text
  , _assetInfoAssetNameAscii        :: !(Maybe Text)
  , _assetInfoAssetStandards        :: !AssetStandards
  , _assetInfoBurnTxCount           :: !Integer
  , _assetInfoFingerprint           :: !Text
  , _assetInfoFirstMintTime         :: !POSIXTime
  , _assetInfoFirstMintTx           :: !Text
  , _assetInfoLatestMintTxMetadata  :: !(Maybe Aeson.Value)
  , _assetInfoMintTxCount           :: !Natural
  , _assetInfoTokenRegistryMetadata :: !(Maybe TokenRegistryMetadata)
  , _assetInfoTotalSupply           :: !Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetInfo", CamelToSnake]] AssetInfo

data AssetTx = AssetTx
  { _assetTxBlockHeight :: !BlockHeight
  , _assetTxEpochNo     :: !EpochNo
  , _assetTxTxHash      :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetTx", CamelToSnake]] AssetTx

data AssetUtxo = AssetUtxo
  { _assetUtxoAddress :: !Text
  , _assetUtxoAmount  :: !Integer
  , _assetUtxoIndex   :: !Natural
  , _assetUtxoTxHash  :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetUtxo", CamelToSnake]] AssetUtxo

data AssetInPolicy = AssetInPolicy
  { _assetInPolicyName     :: !Text
  , _assetInPolicyQuantity :: !Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetInPolicy", LowerFirst]] AssetInPolicy

data PolicyUtxo = PolicyUtxo
  { _policyUtxoAddress :: !Text
  , _policyUtxoAssets  :: ![AssetInPolicy]
  , _policyUtxoIndex   :: !Natural
  , _policyUtxoTxHash  :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_policyUtxo", CamelToSnake]] PolicyUtxo

data MintingTx = MintingTx
  { _mintingTxBlockTimestamp :: !POSIXTime
  , _mintingTxMetadata       :: !(Maybe Aeson.Value)
  , _mintingTxMintAmount     :: !Integer
  , _mintingTxTxHash         :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_mintingTx", CamelToSnake]] MintingTx
