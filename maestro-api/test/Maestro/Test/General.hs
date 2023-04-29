module Maestro.Test.General where

import           Data.Aeson           (eitherDecode)
import           Test.Hspec
import           Text.RawString.QQ

import           Data.ByteString.Lazy (ByteString)
import           Maestro.Types

spec_general :: Spec
spec_general = do
  it "parsing /chain-tip sample" $ do
    eitherDecode chainTipSample
    `shouldBe`
    Right chainTipExpected

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
