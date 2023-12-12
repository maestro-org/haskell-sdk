-- | Module to query for /"DeFi Markets"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/category/defi-market-api).

module Maestro.Client.V1.DefiMarkets (
    pricesFromDex,
    pairsFromDex
  ) where

import           Maestro.API.V1
import           Maestro.API.V1.DefiMarkets
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.Common     (Order)
import           Maestro.Types.V1         (Resolution, Dex, TaggedText, PairOfDexTokens,DexPairResponse, OHLCCandleInfo)
import           Servant.API.Generic
import           Servant.Client

defiMarketsClient :: MaestroEnv 'V1 -> DefiMarketsAPI (AsClientT IO)
defiMarketsClient = fromServant . defiMarkets . apiV1Client

-- | Returns a list of OHLC formatted candles from the desired dex
pricesFromDex ::
  MaestroEnv 'V1 ->
  Dex ->
  TaggedText PairOfDexTokens ->
  Maybe Resolution ->
  Maybe Order ->
  IO [OHLCCandleInfo]
pricesFromDex = dexOHLC . defiMarketsClient

-- | Returns a the list of pairs supported by the dex
pairsFromDex ::
  MaestroEnv 'V1 ->
  Dex ->
  IO DexPairResponse
pairsFromDex = dexPairs . defiMarketsClient
