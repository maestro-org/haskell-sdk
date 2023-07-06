module Maestro.Types.V1.Common
  ( LastUpdated (..),
    Asset (..),
    v1AssetToV0,
    UtxoData (..),
    v1UtxoToV0,
    Utxos (..),
    module Maestro.Types.Common
  )
where

import           Data.Text                         (Text)
import qualified Data.Text                         as T (splitAt)
import           Deriving.Aeson
import           GHC.Natural                       (Natural)
import           Maestro.Client.V1.Core.Pagination (HasCursor (..))
import           Maestro.Types.Common
import qualified Maestro.Types.V0                  as V0 (Asset (..), Utxo (..))

-- | Details of the most recent block processed by the indexer (aka chain tip); that is, the data returned is correct as of this block in time.
data LastUpdated = LastUpdated
  { _lastUpdatedBlockHash :: !BlockHash
  -- ^ Hash of the latest block.
  , _lastUpdatedBlockSlot :: !SlotNo
  -- ^ Slot number for the tip.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_lastUpdated", CamelToSnake]] LastUpdated

-- | Representation of asset in an UTxO.
data Asset = Asset
  { _assetAmount :: !Integer
  , _assetUnit   :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_asset", CamelToSnake]] Asset

-- | Convert @V1@ API version `Asset` type into corresponding @V0@ type.
v1AssetToV0 :: Asset -> V0.Asset
v1AssetToV0 Asset {..} = V0.Asset {
    V0._assetQuantity = _assetAmount
  , V0._assetUnit =
      if _assetUnit == "lovelace" then _assetUnit
      else
        let (policyId, tokenName) = T.splitAt 56 _assetUnit
        in policyId <> "#" <> tokenName
  }

-- | Transaction output.
data UtxoData = UtxoData
  { _utxoDataAddress         :: !Text,
    _utxoDataAssets          :: ![Asset],
    _utxoDataDatum           :: !(Maybe DatumOption),
    _utxoDataIndex           :: !Natural,
    _utxoDataReferenceScript :: !(Maybe Script),
    _utxoDataTxHash          :: !Text,
    _utxoDataSlot            :: !SlotNo,
    _utxoDataTxoutCbor       :: !(Maybe (HexStringOf TxOutCbor))

  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoData", CamelToSnake]] UtxoData

-- | Convert @V1@ API version `UtxoData` type into corresponding @V0@ type.
v1UtxoToV0 :: UtxoData -> V0.Utxo
v1UtxoToV0 UtxoData {..} = V0.Utxo {
    V0._utxoAddress = _utxoDataAddress
  , V0._utxoAssets = map v1AssetToV0 _utxoDataAssets
  , V0._utxoDatum = _utxoDataDatum
  , V0._utxoIndex = _utxoDataIndex
  , V0._utxoReferenceScript = _utxoDataReferenceScript
  , V0._utxoTxHash = _utxoDataTxHash
  , V0._utxoTxoutCbor = _utxoDataTxoutCbor
  }

-- | Transaction Outputs
data Utxos = Utxos
  { _utxosData        :: ![UtxoData],
    _utxosLastUpdated :: !LastUpdated,
    _utxosNextCursor  :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxos", CamelToSnake]] Utxos

instance HasCursor Utxos where
  type CursorData Utxos = [UtxoData]
  getNextCursor utxos = _utxosNextCursor utxos
  getCursorData utxos = _utxosData utxos
