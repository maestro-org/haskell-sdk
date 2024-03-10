-- | Module to define types for /\"Assets\"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/Indexer-API/Assets/asset-addresses).
module Maestro.Types.V1.Assets (
  TokenRegistryMetadata (..),
  AssetInfo (..),
  TimestampedAssetInfo (..),
) where

import Data.Text (Text)
import Data.Word (Word64)
import Deriving.Aeson
import Maestro.Types.V1.Common

-- | Token registry metadata
data TokenRegistryMetadata = TokenRegistryMetadata
  { tokenRegistryMetadataName :: !Text
  -- ^ Asset name.
  , tokenRegistryMetadataDescription :: !Text
  -- ^ Asset description.
  , tokenRegistryMetadataDecimals :: !(Maybe Word64)
  -- ^ Recommended value for decimal places.
  , tokenRegistryMetadataLogo :: !(Maybe Text)
  -- ^ Base64 encoded logo PNG associated with the asset.
  , tokenRegistryMetadataUrl :: !(Maybe Text)
  -- ^ URL associated with the asset.
  , tokenRegistryMetadataTicker :: !(Maybe Text)
  -- ^ Asset ticker.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "tokenRegistryMetadata", CamelToSnake]] TokenRegistryMetadata

-- | Information about a specific Cardano native-asset.
data AssetInfo = AssetInfo
  { assetInfoTokenRegistryMetadata :: !(Maybe TokenRegistryMetadata)
  -- ^ See `TokenRegistryMetadata`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "assetInfo", CamelToSnake]] AssetInfo

-- | Timestamped `AssetInfo` response.
data TimestampedAssetInfo = TimestampedAssetInfo
  { timestampedAssetInfoData :: !AssetInfo
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
