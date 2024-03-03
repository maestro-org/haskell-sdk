-- | Module to query for /"DeFi Markets"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/category/defi-market-api).
module Maestro.Client.V1.DefiMarkets (
  pricesFromDex,
  pairsFromDex,
) where

import Data.Time (Day)
import Data.Word (Word64)
import Maestro.API.V1
import Maestro.API.V1.DefiMarkets
import Maestro.Client.Env
import Maestro.Client.V1.Core
import Maestro.Types.Common (Order)
import Maestro.Types.V1 (Dex, DexPairResponse, OHLCCandleInfo, PairOfDexTokens, Resolution, TaggedText)
import Servant.API.Generic
import Servant.Client

defiMarketsClient :: MaestroEnv 'V1 -> DefiMarketsAPI (AsClientT IO)
defiMarketsClient = fromServant . defiMarkets . apiV1Client

-- | Returns a list of OHLC formatted candles from the desired dex
pricesFromDex ::
  MaestroEnv 'V1 ->
  -- | Dex.
  Dex ->
  -- | Pair.
  TaggedText PairOfDexTokens ->
  -- | Resolution.
  Maybe Resolution ->
  -- | From day.
  Maybe Day ->
  -- | To day.
  Maybe Day ->
  -- | Limit. Default value is @5000@.
  Maybe Word64 ->
  -- | Order.
  Maybe Order ->
  IO [OHLCCandleInfo]
pricesFromDex = dexOHLC . defiMarketsClient

-- | Returns a the list of pairs supported by the dex
pairsFromDex ::
  MaestroEnv 'V1 ->
  Dex ->
  IO DexPairResponse
pairsFromDex = dexPairs . defiMarketsClient
