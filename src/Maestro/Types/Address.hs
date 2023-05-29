module Maestro.Types.Address where

import           Deriving.Aeson

newtype AddressTxCount = AddressTxCount
  { _addressTxCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_addressTx", CamelToSnake]] AddressTxCount

data UtxoRef = UtxoRef
  { _utxoRefIndex  :: !Int
  , _utxoRefTxHash :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoRef", CamelToSnake]] UtxoRef
