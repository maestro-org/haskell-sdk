module Maestro.Types.Assets where

import qualified Data.Aeson           as Aeson
import           Deriving.Aeson
import           Maestro.Types.Common (MaestroAsset)

data TokenRegistryMetadata = TokenRegistryMetadata
  { _tokenRegMetDecimals    :: !Integer
  , _tokenRegMetDescription :: !String
  , _tokenRegMetLogo        :: !String
  , _tokenRegMetName        :: !String
  , _tokenRegMetTicker      :: !String
  , _tokenRegMetUrl         :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_tokenRegMet", CamelToSnake]] TokenRegistryMetadata


newtype AssetStandards = AssetStandards
  { _assetStandardsCip25Metadata :: Maybe Aeson.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetStandards", CamelToSnake]] AssetStandards


data AssetInfo = AssetInfo
  { _assetInfoAssetName             :: !String
  , _assetInfoAssetNameAscii        :: !String
  , _assetInfoAssetStandards        :: !AssetStandards
  , _assetInfoBurnTxCount           :: !Integer
  , _assetInfoFingerprint           :: !String
  , _assetInfoFirstMintTime         :: !Integer
  , _assetInfoFirstMintTx           :: !String
  , _assetInfoLatestMintTxMetadata  :: !(Maybe Aeson.Value)
  , _assetInfoMintTxCount           :: !Integer
  , _assetInfoTokenRegistryMetadata :: !(Maybe TokenRegistryMetadata)
  , _assetInfoTotalSupply           :: !Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetInfo", CamelToSnake]] AssetInfo

data AssetTx = AssetTx
  { _assetTxBlockHeight :: !Integer
  , _assetTxEpochNo     :: !Integer
  , _assetTxTxHash      :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetTx", CamelToSnake]] AssetTx

data AssetUtxo = AssetUtxo
  { _assetUtxoAddress :: !String
  , _assetUtxoAssets  :: !(Maybe [MaestroAsset])
  , _assetUtxoIndex   :: !Int
  , _assetUtxoTxHash  :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetUtxo", CamelToSnake]] AssetUtxo

data MintingTx = MintingTx
  { _mintingTxBlockTimestamp :: !Integer
  , _mintingTxMetadata       :: !(Maybe Aeson.Value)
  , _mintingTxMintAmount     :: !Integer
  , _mintingTxTxHash         :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_mintingTx", CamelToSnake]] MintingTx
