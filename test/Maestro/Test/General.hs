module Maestro.Test.General where

import           Data.Aeson                     (eitherDecode)
import qualified Data.Map.Strict                as Map
import           Data.Ratio                     ((%))
import           Data.Time                      (LocalTime (..), midnight)
import           Test.Hspec
import           Text.RawString.QQ

import           Data.ByteString.Lazy           (ByteString)
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import           Maestro.Types.V0

spec_general :: Spec
spec_general = do
  it "parsing /chain-tip sample" $ do
    eitherDecode chainTipSample
    `shouldBe`
    Right chainTipExpected

  it "parsing /system-start sample" $ do
    eitherDecode systemStartSample
    `shouldBe`
    Right systemStartExpected

  it "parsing /era-history sample" $ do
    eitherDecode eraHistorySample
    `shouldBe`
    Right eraHistoryExpected

  it "parsing /protocol-params sample" $ do
    eitherDecode protocolParamsSample
    `shouldBe`
    Right protocolParamsExpected

chainTipSample :: ByteString
chainTipSample = [r|
{
  "block_hash": "2de4cd25f0a18228ce6451950793c54f6c556a53a5625c7fe80721f77c8b5fa8",
  "slot": 84899112,
  "height": 8402752
}
|]

chainTipExpected :: ChainTip
chainTipExpected = ChainTip
  { _chainTipBlockHash = BlockHash "2de4cd25f0a18228ce6451950793c54f6c556a53a5625c7fe80721f77c8b5fa8"
  , _chainTipSlot      = 84899112
  , _chainTipHeight    = 8402752
  }

systemStartSample :: ByteString
systemStartSample = [r|
{
  "time": "2022-06-01 00:00:00"
}
|]

systemStartExpected :: SystemStart
systemStartExpected = SystemStart $ LocalTime (fromOrdinalDate 2022 152) midnight

eraHistorySample :: ByteString
eraHistorySample = [r|
[
  {
    "end": {
      "epoch": 1,
      "slot": 2,
      "time": 3
    },
    "parameters": {
      "epoch_length": 30,
      "safe_zone": 40,
      "slot_length": 50
    },
    "start": {
      "epoch": 1,
      "slot": 2,
      "time": 3
    }
  }
]
|]

eraHistoryExpected :: [EraSummary]
eraHistoryExpected =
  [ EraSummary
      eraBoundSample
      (Just eraBoundSample)
      eraParametersSample
  ]
  where
    eraBoundSample =
      EraBound
        { _eraBoundEpoch = 1
        , _eraBoundSlot = 2
        , _eraBoundTime = 3
        }
    eraParametersSample =
      EraParameters
        { _eraParametersEpochLength = 30
        , _eraParametersSafeZone = 40
        , _eraParametersSlotLength = 50
        }

protocolParamsSample :: ByteString
protocolParamsSample = [r|
{
  "coins_per_utxo_byte": 31,
  "collateral_percentage": 29,
  "cost_models": {
    "plutus:v1": {
      "A": 0,
      "B": 1,
      "C": 2
    },
    "plutus:v2": {
      "D": 3,
      "E": 4,
      "F": 5
    }
  },
  "desired_number_of_pools": 23,
  "max_block_body_size": 11,
  "max_block_header_size": 13,
  "max_collateral_inputs": 3,
  "max_execution_units_per_block": {
    "memory": 3,
    "steps": 4
  },
  "max_execution_units_per_transaction": {
    "memory": 1,
    "steps": 2
  },
  "max_tx_size": 15,
  "max_value_size": 27,
  "min_fee_coefficient": 9,
  "min_fee_constant": 7,
  "min_pool_cost": 25,
  "monetary_expansion": "7/5",
  "pool_deposit": 19,
  "pool_influence": "5/3",
  "pool_retirement_epoch_bound": 21,
  "prices": {
    "memory": "5/3",
    "steps": "7/5"
  },
  "protocol_version": {
    "major": 1,
    "minor": 2
  },
  "stake_key_deposit": 17,
  "treasury_expansion": "9/7"
}
|]

protocolParamsExpected :: ProtocolParameters
protocolParamsExpected = ProtocolParameters
  { _protocolParametersProtocolVersion = ProtocolVersion 1 2
  , _protocolParametersMinFeeConstant = 7
  , _protocolParametersMinFeeCoefficient = 9
  , _protocolParametersMaxBlockBodySize = 11
  , _protocolParametersMaxBlockHeaderSize = 13
  , _protocolParametersMaxTxSize = 15
  , _protocolParametersStakeKeyDeposit = 17
  , _protocolParametersPoolDeposit = 19
  , _protocolParametersPoolRetirementEpochBound = 21
  , _protocolParametersDesiredNumberOfPools = 23
  , _protocolParametersPoolInfluence = maestroRationalSample
  , _protocolParametersMonetaryExpansion = maestroRationalSample'
  , _protocolParametersTreasuryExpansion = MaestroRational $ 9 % 7
  , _protocolParametersMinPoolCost = 25
  , _protocolParametersPrices = MemoryStepsWith maestroRationalSample maestroRationalSample'
  , _protocolParametersMaxExecutionUnitsPerTransaction = MemoryStepsWith 1 2
  , _protocolParametersMaxExecutionUnitsPerBlock = MemoryStepsWith 3 4
  , _protocolParametersMaxValueSize = 27
  , _protocolParametersCollateralPercentage = 29
  , _protocolParametersMaxCollateralInputs = 3
  , _protocolParametersCoinsPerUtxoByte = 31
  , _protocolParametersCostModels = CostModels
    { _costModelsPlutusV1 =  CostModel $ Map.fromList [("A", 0), ("B", 1), ("C", 2)]
    , _costModelsPlutusV2 = CostModel $ Map.fromList [("D", 3), ("E", 4), ("F", 5)]
    }
  }
  where
    maestroRationalSample = MaestroRational $ 5 % 3
    maestroRationalSample' = MaestroRational $ 7 % 5
