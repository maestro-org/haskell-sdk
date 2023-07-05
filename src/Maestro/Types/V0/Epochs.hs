module Maestro.Types.V0.Epochs ( EpochInfoFees (..), CurrentEpochInfo (..), EpochInfo (..) ) where

import           Data.Aeson              (FromJSON (parseJSON), toEncoding,
                                          toJSON, withText)
import           Data.Text               (unpack)
import           Data.Time.Clock.POSIX   (POSIXTime)
import           Data.Word               (Word64)
import           Deriving.Aeson
import           Maestro.Types.V0.Common
import           Numeric.Natural         (Natural)
import           Text.Read               (readMaybe)

-- | Sum of all the fees within the epoch in lovelaces.
newtype EpochInfoFees = EpochInfoFees Natural
  deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

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

-- | Current epoch information.
data CurrentEpochInfo = CurrentEpochInfo
  { -- | Current epoch number.
    _currentEpochInfoEpochNo   :: !EpochNo
  , -- | Sum of all the fees within the epoch in lovelaces.
    _currentEpochInfoFees      :: !EpochInfoFees
  , -- | Number of transactions within the epoch.
    _currentEpochInfoTxCount   :: !Word64
  , -- | Number of blocks within the epoch.
    _currentEpochInfoBlkCount  :: !Word64
  , -- | Start time of the epoch in UNIX time.
    _currentEpochInfoStartTime :: !POSIXTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_currentEpochInfo", CamelToSnake]] CurrentEpochInfo

-- | Epoch information.
data EpochInfo = EpochInfo
  { -- | Epoch number.
    _epochInfoEpochNo   :: !EpochNo
  , -- | Sum of all the fees within the epoch in lovelaces.
    _epochInfoFees      :: !EpochInfoFees
  , -- | Number of transactions within the epoch.
    _epochInfoTxCount   :: !Word64
  , -- | Number of blocks within the epoch.
    _epochInfoBlkCount  :: !Word64
  , -- | Start time of the epoch in UNIX time.
    _epochInfoStartTime :: !POSIXTime
  , -- | End time of the epoch in UNIX time.
    _epochInfoEndTime   :: !POSIXTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "_epochInfo", CamelToSnake]] EpochInfo
