{-# LANGUAGE QuasiQuotes #-}
module Maestro.Test.Transaction where

import           Data.Aeson           (eitherDecode)
import           Test.Hspec
import           Text.RawString.QQ

import           Data.ByteString.Lazy (ByteString)
import           Maestro.Types

spec_pool :: Spec
spec_pool = do
  it "parsing /transaction/tx_hash/cbor sample" $ do
    eitherDecode txCborSample
    `shouldBe`
    Right txCborExpected

  it "parsing /transaction/tx_hash/outputs/index/address sample" $ do
    eitherDecode txAddressSample
    `shouldBe`
    Right txAddressExpected

txCborSample :: ByteString
txCborSample = [r|
  {
    "cbor": "84a4008382582038dc2ac77bac948bdc623be..."
  }
|]

txCborExpected :: TxCbor
txCborExpected =  TxCbor{ _txCbor = "84a4008382582038dc2ac77bac948bdc623be..."}


txAddressSample :: ByteString
txAddressSample = [r|
  {
    "address": "addr1wxgg25t3tk30jqzl2elqz94lzgmr8a9c9m5z902ds5sjh0g7uaj8z"
  }
|]

txAddressExpected :: TxAddress
txAddressExpected = TxAddress {_txAddress = "addr1wxgg25t3tk30jqzl2elqz94lzgmr8a9c9m5z902ds5sjh0g7uaj8z"}
