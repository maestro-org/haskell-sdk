{-# LANGUAGE DeriveDataTypeable #-}

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
  deriving stock (Eq, Ord, Generic, Data, Typeable, Enum, Bounded)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "Res"]] Resolution

-- >>> show Res1mo
-- "1mo"
instance Show Resolution where
  show = toConstr >>> show >>> drop 3

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
  , ohlcCandleInfoCount :: !Natural
  , ohlcCandleInfoTimestamp :: !UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "ohlcCandleInfo", CamelToSnake]] OHLCCandleInfo
