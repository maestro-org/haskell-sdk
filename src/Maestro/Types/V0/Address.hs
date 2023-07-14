module Maestro.Types.V0.Address where

import           Data.Text      (Text)
import           Deriving.Aeson
import           GHC.Natural    (Natural)

newtype AddressTxCount = AddressTxCount
  { _addressTxCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_addressTx", CamelToSnake]] AddressTxCount

data UtxoRef = UtxoRef
  { _utxoRefIndex  :: !Natural
  , _utxoRefTxHash :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoRef", CamelToSnake]] UtxoRef
