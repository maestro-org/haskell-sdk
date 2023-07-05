module Maestro.Types.V1.Common
  ( LastUpdated (..),
    Asset (..),
    UtxoData (..),
    Utxos (..),
    module Maestro.Types.Common
  )
where

import           Data.Text                         (Text)
import           Deriving.Aeson
import           GHC.Natural                       (Natural)
import           Maestro.Client.V1.Core.Pagination (HasCursor (..))
import           Maestro.Types.Common

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
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_asset", CamelToSnake]] Asset

-- | Transaction output.
data UtxoData = UtxoData
  { _utxoDataAddress         :: !Text,
    _utxoDataAssets          :: ![Asset],
    _utxoDataDatum           :: !(Maybe DatumOption),
    _utxoDataIndex           :: !Natural,
    _utxoDataReferenceScript :: !(Maybe Script),
    _utxoDataTxHash          :: !Text,
    _utxoDataSlot            :: !SlotNo,
    _utxoDataTxoutCbor       :: !(Maybe TxCbor)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoData", CamelToSnake]] UtxoData

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
