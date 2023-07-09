-- | Module to define types for /\"Datum\"/ endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/datum).

module Maestro.Types.V1.Datum
  ( Datum (..)
  ) where

import           Data.Aeson              (Value)
import           Data.Text               (Text)
import           Deriving.Aeson
import           Maestro.Types.V1.Common (LowerFirst)

-- | Details of datum when queried by it's hash.
data Datum = Datum
  { _datumBytes :: !Text
  -- ^ Hex encoded datum CBOR bytes.
  , _datumJson  :: !Value
  -- ^ JSON representation of the datum.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_datum", LowerFirst]] Datum
