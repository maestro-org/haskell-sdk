-- | Module to define types for /"DeFi Markets"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/category/defi-market-api).

module Maestro.Types.V1.DefiMarkets (
    Dex(..),
    PairOfDexTokens,
    Resolution (..),
    DexPairResponse(..),
    DexPairInfo (..),
    OHLCCandleInfo (..)
  ) where

import qualified Data.Text as T
import           Deriving.Aeson
import           Maestro.Types.V1.Common
import qualified Data.Aeson           as Aeson
import           Servant.API

-- | Denotes which dex to use
data Dex = Minswap
  deriving stock (Eq, Ord, Generic)

instance Show Dex where
    show Minswap = "minswap"

instance ToHttpApiData Dex where
  toQueryParam = T.pack . show

-- | Because there is only one dex at the moment, the derivation returns "[]". This can be removed once support for a new dex is added.
instance FromJSON Dex where
  parseJSON (Aeson.String "minswap") = return Minswap
  parseJSON _ = fail "Expecting oneof [minswap]"

-- | Token Pair that is queried
type PairOfDexTokens = "Token pair to look for. Format: XXX-YYY"

-- | Time resolution for OHLC Candles
data Resolution = Res1m | Res5m | Res15m | Res30m | Res1h | Res4h | Res1d | Res1w | Res1mo
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "Res"]] Resolution

instance Show Resolution where
  show Res1m  = "1m"
  show Res5m  = "5m"
  show Res15m = "15m"
  show Res30m = "30m"
  show Res1h  = "1h"
  show Res4h  = "4h"
  show Res1d  = "1d"
  show Res1w  = "1w"
  show Res1mo = "1mo"

instance ToHttpApiData Resolution where
  toQueryParam = T.pack . show

data DexPairInfo = DexPairInfo
  { dexPairInfoCoinAAssetName :: TokenName
  , dexPairInfoCoinAPolicy    :: PolicyId
  , dexPairInfoCoinBAssetName :: TokenName
  , dexPairInfoCoinBPolicy    :: PolicyId
  , dexPairInfoPair           :: String
}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "dexPairInfo", CamelToSnake]] DexPairInfo

data DexPairResponse = DexPairResponse
  { dexPairResponseDex   :: Dex
  , dexPairResponsePairs :: [DexPairInfo]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "dexPairResponse", LowerFirst]] DexPairResponse

-- | Candle data according to the [OHLC format](https://en.wikipedia.org/wiki/Open-high-low-close_chart)
data OHLCCandleInfo = OHLCCandleInfo
  { ohlcCandleInfoCoinAClose  :: Double
  , ohlcCandleInfoCoinAHigh   :: Double
  , ohlcCandleInfoCoinALow    :: Double
  , ohlcCandleInfoCoinAOpen   :: Double
  , ohlcCandleInfoCoinAVolume :: Double
  , ohlcCandleInfoCoinBClose  :: Double
  , ohlcCandleInfoCoinBHigh   :: Double
  , ohlcCandleInfoCoinBLow    :: Double
  , ohlcCandleInfoCoinBOpen   :: Double
  , ohlcCandleInfoCoinBVolume :: Double
  , ohlcCandleInfoCount       :: Integer
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "ohlcCandleInfo", CamelToSnake]] OHLCCandleInfo
