module Maestro.API.V1.DefiMarkets where

import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data DefiMarketsAPI route = DefiMarketsAPI
  {
    dexOHLC
      :: route
      :- "ohlc"
      :> Capture "dex" Dex
      :> Capture "pair" (TaggedText PairOfDexTokens)
      :> QueryParam "resolution" Resolution
      :> QueryParam "sort" Order
      :> Get '[JSON] [OHLCCandleInfo]

  , dexPairs
      :: route
      :- Capture "dex" Dex
      :> Get '[JSON] DexPairResponse

  } deriving (Generic)
