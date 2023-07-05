module Maestro.Types.V0.Common
  ( Utxo (..),
    module Maestro.Types.Common
  )
where

import           Data.Text            (Text)
import           Deriving.Aeson
import           GHC.Natural          (Natural)
import           Maestro.Types.Common

-- | Transaction output
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
