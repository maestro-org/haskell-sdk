module Maestro.Types.V0.Transactions
  ( TxCbor (..),
    UtxoAddress (..),
  )
where

import           Data.Text            (Text)
import           Deriving.Aeson
import           Maestro.Types.Common (LowerFirst)

newtype TxCbor = TxCbor { _txCbor :: Text }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_tx", LowerFirst]] TxCbor

newtype UtxoAddress = UtxoAddress { _utxoAddressAddress :: Text }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoAddress", LowerFirst]] UtxoAddress

