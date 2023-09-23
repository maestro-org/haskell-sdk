-- | Module to define types for /\"Datum\"/ endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/datum).

module Maestro.Types.V1.Datum
  ( Datum (..)
  , TimestampedDatum (..)
  ) where

import           Data.Aeson              (Value)
import           Data.Text               (Text)
import           Deriving.Aeson
import           Maestro.Types.V1.Common (IsTimestamped (..), LastUpdated,
                                          LowerFirst)

-- | Details of datum when queried by it's hash.
data Datum = Datum
  { datumBytes :: !Text
  -- ^ Hex encoded datum CBOR bytes.
  , datumJson  :: !Value
  -- ^ JSON representation of the datum.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "datum", LowerFirst]] Datum

-- | Timestamped `Datum` response.
data TimestampedDatum = TimestampedDatum
  { timestampedDatumData        :: !Datum
  -- ^ See `Datum`.
  , timestampedDatumLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "timestampedDatum", CamelToSnake]] TimestampedDatum

instance IsTimestamped TimestampedDatum where
  type TimestampedData TimestampedDatum = Datum
  getTimestampedData = timestampedDatumData
  getTimestamp = timestampedDatumLastUpdated
