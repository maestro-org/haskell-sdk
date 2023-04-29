module Maestro.Types.Assets where

import qualified Data.Aeson           as Aeson
import           Deriving.Aeson
import           Maestro.Types.Common (MaestroAsset)

data MaestroTokenRegistryMetadata = MaestroTokenRegistryMetadata
  { _maeTokenRegDecimals    :: !Integer
  , _maeTokenRegDescription :: !String
  , _maeTokenRegLogo        :: !String
  , _maeTokenRegName        :: !String
  , _maeTokenRegTicker      :: !String
  , _maeTokenRegUrl         :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maeTokenReg", CamelToSnake]] MaestroTokenRegistryMetadata


newtype MaestroAssetStandard = MaestroAssetStandard
  { _maeAssetStdCip25Metadata :: Maybe Aeson.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maeAssetStd", CamelToSnake]] MaestroAssetStandard


data MaestroAssetInfo = MaestroAssetInfo
  { _maeAssetInfoAssetName             :: !String
  , _maeAssetInfoAssetNameAscii        :: !String
  , _maeAssetInfoAssetStandards        :: !MaestroAssetStandard
  , _maeAssetInfoBurnTxCount           :: !Integer
  , _maeAssetInfoFingerprint           :: !String
  , _maeAssetInfoFirstMintTime         :: !Integer
  , _maeAssetInfoFirstMintTx           :: !String
  , _maeAssetInfoLatestMintTxMetadata  :: !(Maybe Aeson.Value)
  , _maeAssetInfoMintTxCount           :: !Integer
  , _maeAssetInfoTokenRegistryMetadata :: !(Maybe MaestroTokenRegistryMetadata)
  , _maeAssetInfoTotalSupply           :: !Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maeAssetInfo", CamelToSnake]] MaestroAssetInfo

data MaestroAssetTx = MaestroAssetTx
  { _maeAssetBlockHeight :: !Integer
  , _maeAssetEpochNo     :: !Integer
  , _maeAssetTxHash      :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maeAsset", CamelToSnake]] MaestroAssetTx

data MaestroAssetUtxo = MaestroAssetUtxo
  { _maeAssetUtxoAddress :: !String
  , _maeAssetUtxoAssets  :: !(Maybe [MaestroAsset])
  , _maeAssetUtxoIndex   :: !Int
  , _maeAssetUtxoTxHash  :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maeAssetUtxo", CamelToSnake]] MaestroAssetUtxo

data MaestroAssetUpdates = MaestroAssetUpdates
  { _maeAssetUpdateBlockTimestamp :: !Integer
  , _maeAssetUpdateMetadata       :: !(Maybe Aeson.Value)
  , _maeAssetUpdateMintAmount     :: !Integer
  , _maeAssetUpdateTxHash         :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maeAssetUpdate", CamelToSnake]] MaestroAssetUpdates
