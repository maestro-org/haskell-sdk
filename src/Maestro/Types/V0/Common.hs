module Maestro.Types.V0.Common
  ( AssetId (..),
    Asset (..),
    Utxo (..),
    module Maestro.Types.Common
  )
where

import           Data.Text            (Text)
import           Deriving.Aeson
import           GHC.Natural          (Natural)
import           Maestro.Types.Common
import           Servant.API          (FromHttpApiData, ToHttpApiData)

-- | Concatenation of hex encoded policy ID and hex encoded asset name.
newtype AssetId = AssetId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

-- | Representation of asset in an UTxO.
data Asset = Asset
  { _assetQuantity :: !Integer
  , _assetUnit     :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_asset", CamelToSnake]] Asset

-- | Transaction output.
data Utxo = Utxo
  { _utxoAddress         :: !(Bech32StringOf Address),
    _utxoAssets          :: ![Asset],
    _utxoDatum           :: !(Maybe DatumOption),
    _utxoIndex           :: !Natural,
    _utxoReferenceScript :: !(Maybe Script),
    _utxoTxHash          :: !Text,
    _utxoTxoutCbor       :: !(Maybe (HexStringOf TxOutCbor))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxo", CamelToSnake]] Utxo
