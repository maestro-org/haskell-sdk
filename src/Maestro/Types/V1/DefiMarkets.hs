{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

-- | Module to define types for /"DeFi Markets"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/category/defi-market-api).
module Maestro.Types.V1.DefiMarkets (
  Dex (..),
  PairOfDexTokens,
  Resolution (..),
  DexPairResponse (..),
  DexPairInfo (..),
  OHLCCandleInfo (..),
) where

import Control.Arrow ((>>>))
import Data.Data (Data (toConstr))
import qualified Data.Text as T
import Data.Time (UTCTime)
import Deriving.Aeson
import GHC.Natural (Natural)
import Maestro.Types.V1.Common
import Servant.API
import Type.Reflection (Typeable)

-- | Denotes which dex to use
data Dex = Minswap | GeniusYield
  deriving stock (Eq, Ord, Generic, Enum, Bounded)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToKebab]] Dex

instance Show Dex where
  show Minswap = "minswap"
  show GeniusYield = "genius-yield"

instance ToHttpApiData Dex where
  toQueryParam = T.pack . show

instance FromHttpApiData Dex where
  parseQueryParam = \case
    "minswap" -> Right Minswap
    "genius-yield" -> Right GeniusYield
    _ -> Left "Invalid Dex"

-- | Token Pair that is queried
type PairOfDexTokens = "Token pair to look for. Format: XXX-YYY"

-- | Time resolution for OHLC Candles
data Resolution = Res1m | Res5m | Res15m | Res30m | Res1h | Res4h | Res1d | Res1w | Res1mo
  deriving stock (Eq, Ord, Generic, Data, Typeable, Enum, Bounded)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "Res"]] Resolution

-- >>> show Res1mo
-- "1mo"
instance Show Resolution where
  show = toConstr >>> show >>> drop 3

instance ToHttpApiData Resolution where
  toQueryParam = T.pack . show

instance FromHttpApiData Resolution where
  -- Could have used `find` in combination with `[minBound..maxBound]` to make it more versatile.
  parseQueryParam = \case
    "1m" -> Right Res1m
    "5m" -> Right Res5m
    "15m" -> Right Res15m
    "30m" -> Right Res30m
    "1h" -> Right Res1h
    "4h" -> Right Res4h
    "1d" -> Right Res1d
    "1w" -> Right Res1w
    "1mo" -> Right Res1mo
    _ -> Left "Invalid Resolution"

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
  , ohlcCandleInfoCount :: !Natural
  , ohlcCandleInfoTimestamp :: !UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "ohlcCandleInfo", CamelToSnake]] OHLCCandleInfo
