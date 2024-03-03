-- | Module to define types for /"DeFi Markets"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/category/defi-market-api).
module Maestro.Types.V1.DefiMarkets (
  Dex (..),
  PairOfDexTokens,
  Resolution (..),
  DexPairResponse (..),
  DexPairInfo (..),
  OHLCCandleInfo (..),
) where

import qualified Data.Text as T
import Data.Time (UTCTime)
import Deriving.Aeson
import Maestro.Types.V1.Common
import Servant.API

-- | Denotes which dex to use
data Dex = Minswap | GeniusYield
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToKebab]] Dex

instance Show Dex where
  show Minswap = "minswap"
  show GeniusYield = "genius-yield"

instance ToHttpApiData Dex where
  toQueryParam = T.pack . show

-- | Token Pair that is queried
type PairOfDexTokens = "Token pair to look for. Format: XXX-YYY"

-- | Time resolution for OHLC Candles
data Resolution = Res1m | Res5m | Res15m | Res30m | Res1h | Res4h | Res1d | Res1w | Res1mo
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "Res"]] Resolution

instance Show Resolution where
  show Res1m = "1m"
  show Res5m = "5m"
  show Res15m = "15m"
  show Res30m = "30m"
  show Res1h = "1h"
  show Res4h = "4h"
  show Res1d = "1d"
  show Res1w = "1w"
  show Res1mo = "1mo"

instance ToHttpApiData Resolution where
  toQueryParam = T.pack . show

data DexPairInfo = DexPairInfo
  { dexPairInfoCoinAAssetName :: !TokenName
  , dexPairInfoCoinAPolicy :: !PolicyId
  , dexPairInfoCoinBAssetName :: !TokenName
  , dexPairInfoCoinBPolicy :: !PolicyId
  , dexPairInfoPair :: !T.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "dexPairInfo", CamelToSnake]] DexPairInfo

data DexPairResponse = DexPairResponse
  { dexPairResponseDex :: !Dex
  , dexPairResponsePairs :: ![DexPairInfo]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "dexPairResponse", LowerFirst]] DexPairResponse

-- | Candle data according to the [OHLC format](https://en.wikipedia.org/wiki/Open-high-low-close_chart)
data OHLCCandleInfo = OHLCCandleInfo
  { ohlcCandleInfoCoinAClose :: !Double
  , ohlcCandleInfoCoinAHigh :: !Double
  , ohlcCandleInfoCoinALow :: !Double
  , ohlcCandleInfoCoinAOpen :: !Double
  , ohlcCandleInfoCoinAVolume :: !Double
  , ohlcCandleInfoCoinBClose :: !Double
  , ohlcCandleInfoCoinBHigh :: !Double
  , ohlcCandleInfoCoinBLow :: !Double
  , ohlcCandleInfoCoinBOpen :: !Double
  , ohlcCandleInfoCoinBVolume :: !Double
  , ohlcCandleInfoCount :: !Integer
  , ohlcCandleInfoTimestamp :: !UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "ohlcCandleInfo", CamelToSnake]] OHLCCandleInfo
