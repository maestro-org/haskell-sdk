module Maestro.Types.V0.Common
  ( Asset (..),
    Utxo (..),
    module Maestro.Types.Common
  )
where

import           Data.Text            (Text)
import           Deriving.Aeson
import           GHC.Natural          (Natural)
import           Maestro.Types.Common

-- | Representation of asset in an UTxO.
data Asset = Asset
  { _assetQuantity :: !Integer
  , _assetUnit     :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_asset", CamelToSnake]] Asset

-- | Transaction output.
data Utxo = Utxo
  { _utxoAddress         :: !Text,
    _utxoAssets          :: ![Asset],
    _utxoDatum           :: !(Maybe DatumOption),
    _utxoIndex           :: !Natural,
    _utxoReferenceScript :: !(Maybe Script),
    _utxoTxHash          :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxo", CamelToSnake]] Utxo
