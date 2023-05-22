module Maestro.Types.Epochs ( EpochInfoFees (..), CurrentEpochInfo (..), EpochInfo (..) ) where

import           Data.Aeson            (FromJSON (parseJSON), toEncoding,
                                        toJSON, withText)
import           Data.Text             (unpack)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Word             (Word64)
import           Deriving.Aeson
import           Maestro.Types.Common
import           Numeric.Natural       (Natural)
import           Text.Read             (readMaybe)

newtype EpochInfoFees = EpochInfoFees Natural deriving newtype (Eq, Show)

instance ToJSON EpochInfoFees where
  toEncoding = toEncoding . show
  toJSON = toJSON . show

instance FromJSON EpochInfoFees where
  parseJSON = withText "EpochInfoFees" $ \txt -> either fail pure $ textToNatural $ unpack txt
    where
      textToNatural :: String -> Either String EpochInfoFees
      textToNatural txt = case readMaybe txt :: Maybe Natural of
        Just n  -> Right $ EpochInfoFees n
        Nothing -> Left "Given epoch fees is not a natural number"

data CurrentEpochInfo = CurrentEpochInfo
  { _currentEpochInfoEpochNo   :: !EpochNo
  , _currentEpochInfoFees      :: !EpochInfoFees
  , _currentEpochInfoTxCount   :: !Word64
  , _currentEpochInfoBlkCount  :: !Word64
  , _currentEpochInfoStartTime :: !POSIXTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_currentEpochInfo", CamelToSnake]] CurrentEpochInfo

data EpochInfo = EpochInfo
  { _epochInfoEpochNo   :: !EpochNo
  , _epochInfoFees      :: !EpochInfoFees
  , _epochInfoTxCount   :: !Word64
  , _epochInfoBlkCount  :: !Word64
  , _epochInfoStartTime :: !POSIXTime
  , _epochInfoEndTime   :: !POSIXTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_epochInfo", CamelToSnake]] EpochInfo
