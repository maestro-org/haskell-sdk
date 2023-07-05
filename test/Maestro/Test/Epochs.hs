module Maestro.Test.Epochs where

import           Data.Aeson           (eitherDecode)
import           Test.Hspec
import           Text.RawString.QQ

import           Data.ByteString.Lazy (ByteString)
import           Maestro.Types.V0

spec_general :: Spec
spec_general = do

  it "parsing /epochs/current sample" $ do
    eitherDecode currentEpochSample
    `shouldBe`
    Right currentEpochExpected

  it "parsing /epochs/{epoch_no}/info sample" $ do
    eitherDecode epochInfoSample
    `shouldBe`
    Right epochInfoExpected


currentEpochSample :: ByteString
currentEpochSample = [r|
{
  "epoch_no": 413,
  "fees": "47553352844",
  "tx_count": 127481,
  "blk_count": 6488,
  "start_time": 1684619256
}
|]

currentEpochExpected :: CurrentEpochInfo
currentEpochExpected = CurrentEpochInfo
  { _currentEpochInfoBlkCount  = 6488
  , _currentEpochInfoEpochNo   = 413
  , _currentEpochInfoFees      = EpochInfoFees 47553352844
  , _currentEpochInfoStartTime = 1684619256
  , _currentEpochInfoTxCount   = 127481
  }

epochInfoSample :: ByteString
epochInfoSample = [r|
{
  "epoch_no": 36,
  "fees": "8308727733",
  "tx_count": 18799,
  "blk_count": 21052,
  "start_time": 1669593614,
  "end_time": 1670025570
}
|]

epochInfoExpected :: EpochInfo
epochInfoExpected = EpochInfo
  { _epochInfoEpochNo   = 36
  , _epochInfoFees      = EpochInfoFees 8308727733
  , _epochInfoTxCount   = 18799
  , _epochInfoBlkCount  = 21052
  , _epochInfoStartTime = 1669593614
  , _epochInfoEndTime   = 1670025570
  }



