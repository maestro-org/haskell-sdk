module Maestro.Test.Datum where

import           Data.Aeson           (eitherDecode, object, (.=))
import           Test.Hspec
import           Text.RawString.QQ

import           Data.ByteString.Lazy (ByteString)
import           Maestro.Types.V0

spec_general :: Spec
spec_general = do

  it "parsing /datum/{datum_hash} sample" $ do
    eitherDecode datumByHashSample
    `shouldBe`
    Right datumByHashExpected


datumByHashSample :: ByteString
datumByHashSample = [r|
{
  "bytes": "1a00278530",
  "json": {
    "int": 2590000
  }
}
|]

datumByHashExpected :: Datum
datumByHashExpected = Datum
  { _datumBytes  = "1a00278530"
  , _datumJson   = object ["int" .= (2590000 :: Int)]
  }
