-- | Module to define types for /\"Assets\"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/Indexer-API/Assets/asset-addresses).
module Maestro.Types.V1.Assets (
  TokenRegistryMetadata (..),
  AssetInfo (..),
  TimestampedAssetInfo (..),
  AssetUTxOs (..),
  TimestampedAssetUTxOs (..),
) where

import           Data.Aeson              (Value)
import           Data.Text               (Text)
import           Data.Word               (Word64)
import           Deriving.Aeson
import           GHC.Natural             (Natural)
import           Maestro.Types.V1.Common

-- | Token registry metadata
data TokenRegistryMetadata = TokenRegistryMetadata
  { tokenRegistryMetadataName        :: !Text
  -- ^ Asset name.
  , tokenRegistryMetadataDescription :: !Text
  -- ^ Asset description.
  , tokenRegistryMetadataDecimals    :: !(Maybe Word64)
  -- ^ Recommended value for decimal places.
  , tokenRegistryMetadataLogo        :: !(Maybe Text)
  -- ^ Base64 encoded logo PNG associated with the asset.
  , tokenRegistryMetadataUrl         :: !(Maybe Text)
  -- ^ URL associated with the asset.
  , tokenRegistryMetadataTicker      :: !(Maybe Text)
  -- ^ Asset ticker.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "tokenRegistryMetadata", CamelToSnake]] TokenRegistryMetadata

data Cip68AssetType = CIP68ATReferenceNft | CIP68ATUserNft | CIP68ATUserFt
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "CIP68AT", CamelToSnake]] Cip68AssetType

data Cip68Metadata = Cip68Metadata
  { cip68MetadataExtra    :: !(Maybe Text)
  -- ^ Custom user defined Plutus data CBOR bytes.
  , cip68MetadataMetadata :: !Value
  -- ^ Asset CIP-68 metadata.
  , cip68MetadataPurpose  :: !Cip68AssetType
  -- ^ Purpose.
  , cip68MetadataVersion  :: !Word64
  -- ^ CIP-68 version.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "cip68Metadata", CamelToSnake]] Cip68Metadata

-- | Asset information corresponding to popular standards.
data AssetStandards = AssetStandards
  { assetStandardsCip25Metadata :: !(Maybe Value)
  -- ^ CIP-25 metadata for a specific asset.
  , assetStandardsCip68Metadata :: !(Maybe Cip68Metadata)
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "assetStandards", CamelToSnake]] AssetStandards

-- | Information about a specific Cardano native-asset.
data AssetInfo = AssetInfo
  { assetInfoAssetName             :: !TokenName
  -- ^ Hex encoding of the asset name.
  , assetInfoAssetStandards        :: !AssetStandards
  -- ^ Asset information corresponding to popular standards.
  , assetInfoLatestMintTxMetadata  :: !(Maybe Value)
  -- ^ Metadata of the most recent transaction which minted or burned the asset.
  , assetInfoTokenRegistryMetadata :: !(Maybe TokenRegistryMetadata)
  -- ^ See `TokenRegistryMetadata`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "assetInfo", CamelToSnake]] AssetInfo

-- | Timestamped `AssetInfo` response.
data TimestampedAssetInfo = TimestampedAssetInfo
  { timestampedAssetInfoData        :: !AssetInfo
  -- ^ See `AssetInfo`.
  , timestampedAssetInfoLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedAssetInfo", CamelToSnake]] TimestampedAssetInfo

instance IsTimestamped TimestampedAssetInfo where
  type TimestampedData TimestampedAssetInfo = AssetInfo
  getTimestampedData = timestampedAssetInfoData
  getTimestamp = timestampedAssetInfoLastUpdated

-- | UTxOs that contain a specific asset.
data AssetUTxOs = AssetUTxOs
  { assetUTxOsTxHash  :: !TxHash
  -- ^ Transaction hash.
  , assetUTxOsIndex   :: !TxIndex
  -- ^ Transaction index.
  , assetUTxOsAddress :: !(Bech32StringOf Address)
  -- ^ Address.
  , assetUTxOsAmount  :: !Natural
  -- ^ Amount.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "assetUTxOs", CamelToSnake]] AssetUTxOs

-- | Timestamped `AssetUTxOs` response.
data TimestampedAssetUTxOs = TimestampedAssetUTxOs
  { timestampedAssetUTxOsData        :: ![AssetUTxOs]
  -- ^ See `AssetUTxOs`.
  , timestampedAssetUTxOsLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedAssetUTxOs", CamelToSnake]] TimestampedAssetUTxOs
