{-# LANGUAGE QuasiQuotes #-}
module Maestro.Test.Pool where

import           Data.Aeson           (eitherDecode)
import           Test.Hspec
import           Text.RawString.QQ

import           Data.ByteString.Lazy (ByteString)
import           Maestro.Types.V0

spec_pool :: Spec
spec_pool = do
  it "parsing /pools sample" $ do
    eitherDecode poolsSample
    `shouldBe`
    Right poolsExpected

  it "parsing /pools/pool_id/blocks sample" $ do
    eitherDecode poolBlocksSample
    `shouldBe`
    Right poolBlockExpected

  it "parsing /pools/pool_id/delegators sample" $ do
    eitherDecode poolDelegatorSample
    `shouldBe`
    Right poolDelegatorExpected

  it "parsing /pools/pool_id/history sample" $ do
    eitherDecode poolHistorySample
    `shouldBe`
    Right poolHistoryExpected

  it "parsing /pools/pool_id/info sample" $ do
    eitherDecode poolInfoSample
    `shouldBe`
    Right poolInfoExpected

  it "parsing /pools/pool_id/metadata sample" $ do
    eitherDecode poolMetadataSample
    `shouldBe`
    Right poolMetadataExpected

  it "parsing /pools/pool_id/relays sample" $ do
    eitherDecode poolRelaySample
    `shouldBe`
    Right poolRelayExpected

  it "parsing /pools/pool_id/updates sample" $ do
    eitherDecode poolUpdatesSample
    `shouldBe`
    Right poolUpdatesExpected

poolsSample :: ByteString
poolsSample = [r|
  [
    {
      "pool_id_bech32": "pool12584mjtgz3fhgpx823qht56gycnfnezg6aqqthazv4qdxkd5c46",
      "ticker": "DOLCA"
    },
      {
      "pool_id_bech32": "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r",
      "ticker": "CHOCO"
      }
  ]
|]

poolsExpected :: [PoolListInfo]
poolsExpected =
  [
    PoolListInfo {
      _poolListInfoPoolIdBech32 = "pool12584mjtgz3fhgpx823qht56gycnfnezg6aqqthazv4qdxkd5c46",
      _poolListInfoTicker = Just "DOLCA"
         },

    PoolListInfo {
      _poolListInfoPoolIdBech32 = "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r",
      _poolListInfoTicker = Just "CHOCO"
         }

  ]

poolBlocksSample :: ByteString
poolBlocksSample = [r|
  [
    {
      "epoch_no": 331,
      "epoch_slot": 247198,
      "abs_slot": 57875998,
      "block_height": 7099157,
      "block_hash": "25c48307d94b1d6b98c50482a0a36c6d104c66a5186ce73eac7e9fe4e34a1e4b",
      "block_time": 1649442289
    },
      {
      "epoch_no": 331,
      "epoch_slot": 271486,
      "abs_slot": 57900286,
      "block_height": 7100338,
      "block_hash": "b00aaff6fd95e9718748ef16a6f1787af95970bf8da072c1031f569783001997",
      "block_time": 1649466577
      }
  ]
|]

poolBlockExpected :: [PoolBlock]
poolBlockExpected =
  [
    PoolBlock
      { _poolBlkEpochNo = Just $ EpochNo 331
      , _poolBlkEpochSlot = Just $ EpochSize 247198
      , _poolBlkAbsSlot = Just $ AbsoluteSlot 57875998
      , _poolBlkBlockHeight = BlockHeight 7099157
      , _poolBlkBlockHash = BlockHash "25c48307d94b1d6b98c50482a0a36c6d104c66a5186ce73eac7e9fe4e34a1e4b"
      , _poolBlkBlockTime = 1649442289
      }
    , PoolBlock
      { _poolBlkEpochNo = Just $ EpochNo 331
      , _poolBlkEpochSlot = Just $ EpochSize 271486
      , _poolBlkAbsSlot = Just $ AbsoluteSlot 57900286
      , _poolBlkBlockHeight = BlockHeight 7100338
      , _poolBlkBlockHash = BlockHash "b00aaff6fd95e9718748ef16a6f1787af95970bf8da072c1031f569783001997"
      , _poolBlkBlockTime = 1649466577
      }
  ]

poolDelegatorSample :: ByteString
poolDelegatorSample = [r|
  [
    {
      "stake_address": "stake1uyxylkzz6qcmu823tpfvq2kv4v0eejrh6w3qcfl2t3ax4acmdtchp",
      "amount": 104694974790,
      "active_epoch_no": 288,
      "latest_delegation_tx_hash": "7feeb8988ac7ae488dacf189dd6cb2f52173692dfb45603942fbe8b7a6ccd40b"
    },
    {
      "stake_address": "stake1uyztgnymq9pktsjw7ae9csea4xueymap54gg252wyvsgv8cxscnaz",
      "amount": 4128157247,
      "active_epoch_no": 293,
      "latest_delegation_tx_hash": "e8d1e0a2c9e1d9a33b79db3d4e997a2e797ade35d81ed993936bba409e7c9a1f"
    }
  ]
|]

poolDelegatorExpected :: [DelegatorInfo]
poolDelegatorExpected =
  [
    DelegatorInfo
      { _delegatorStakeAddress  = Just  "stake1uyxylkzz6qcmu823tpfvq2kv4v0eejrh6w3qcfl2t3ax4acmdtchp"
      , _delegatorAmount  = Just 104694974790
      , _delegatorActiveEpochNo = Just $ EpochNo 288
      , _delegatorLatestDelegationTxHash  = Just "7feeb8988ac7ae488dacf189dd6cb2f52173692dfb45603942fbe8b7a6ccd40b"
      }
  ,DelegatorInfo
      { _delegatorStakeAddress  = Just "stake1uyztgnymq9pktsjw7ae9csea4xueymap54gg252wyvsgv8cxscnaz"
      , _delegatorAmount  = Just 4128157247
      , _delegatorActiveEpochNo = Just $ EpochNo 293
      , _delegatorLatestDelegationTxHash  = Just "e8d1e0a2c9e1d9a33b79db3d4e997a2e797ade35d81ed993936bba409e7c9a1f"
      }
  ]

poolHistorySample :: ByteString
poolHistorySample =
  [r|
    [
      {
        "epoch_no": 384,
        "active_stake": 3850078783520,
        "active_stake_pct": "0.01517945984844733700",
        "saturation_pct": "5.4500",
        "block_cnt": 0,
        "delegator_cnt": 23,
        "margin": 0,
        "fixed_cost": 340000000,
        "pool_fees": 0,
        "deleg_rewards": 0,
        "epoch_ros": "0"
      },
      {
        "epoch_no": 385,
        "active_stake": 3851856595953,
        "active_stake_pct": "0.01517156647812579800",
        "saturation_pct": "5.4500",
        "block_cnt": 3,
        "delegator_cnt": 23,
        "margin": 0,
        "fixed_cost": 340000000,
        "pool_fees": 340000000,
        "deleg_rewards": 1447424779,
        "epoch_ros": "2.78058595"
      }
    ]
  |]

poolHistoryExpected :: [PoolHistory]
poolHistoryExpected =
  [
    PoolHistory
      { _poolHstEpochNo = EpochNo 384
      , _poolHstActiveStake = Just $ ActiveStake 3850078783520
      , _poolHstActiveStakePct  = Just "0.01517945984844733700"
      , _poolHstSaturationPct = Just "5.4500"
      , _poolHstBlockCnt  = Just 0
      , _poolHstDelegatorCnt = Just 23
      , _poolHstMargin = Just 0
      , _poolHstFixedCost = 340000000
      , _poolHstPoolFees = 0
      , _poolHstDelegRewards = 0
      , _poolHstEpochRos =  "0"
      }
  , PoolHistory
      { _poolHstEpochNo = EpochNo 385
      , _poolHstActiveStake = Just $ ActiveStake 3851856595953
      , _poolHstActiveStakePct  = Just "0.01517156647812579800"
      , _poolHstSaturationPct = Just "5.4500"
      , _poolHstBlockCnt  = Just 3
      , _poolHstDelegatorCnt = Just 23
      , _poolHstMargin = Just 0
      , _poolHstFixedCost = 340000000
      , _poolHstPoolFees = 340000000
      , _poolHstDelegRewards = 1447424779
      , _poolHstEpochRos = "2.78058595"
      }
  ]

poolInfoSample :: ByteString
poolInfoSample = [r|
  {
    "pool_id_bech32": "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r",
    "pool_id_hex": "552d7f65e42fdc4ed602a657b85bd7623a592d21e5dcf7b045132da8",
    "active_epoch_no": 290,
    "vrf_key_hash": "076102096f5fc37a47ff14390f7320c76ea769183d06881d53dc4ea1be51acae",
    "margin": 0,
    "fixed_cost": 340000000,
    "pledge": 10000000000,
    "reward_addr": "stake1uxtdm5q9j89mmme0l6jt9rsap8cn3vjy3eflptx34s2645ge7r9gp",
    "owners": [
      "stake1uxtdm5q9j89mmme0l6jt9rsap8cn3vjy3eflptx34s2645ge7r9gp"
    ],
    "relays": [
      {
        "dns": null,
        "srv": null,
        "ipv4": "202.182.106.104",
        "ipv6": null,
        "port": 6000
      }
    ],
    "meta_url": "https://git.io/J0eoF",
    "meta_hash": "63c1b74c5489659d8ec374fa9f22f3e185c5ac458ecc69fdc324ba3bbe2c5093",
    "meta_json": {
      "name": "CHOCO Stake Pool",
      "ticker": "CHOCO",
      "homepage": "https://twitter.com/choco_stake",
      "description": "Choco pool description"
    },
    "pool_status": "registered",
    "retiring_epoch": null,
    "op_cert": "cdac991e5a547c86fe869b37eb58c77cc3804d2e592200c835da0b3dbfe8e763",
    "op_cert_counter": 7,
    "active_stake": 3847126072870,
    "sigma": "0.00015093541821247124",
    "block_count": 307,
    "live_pledge": 38784976226,
    "live_stake": 3848841013319,
    "live_delegators": 24,
    "live_saturation": "5.4200"
  }
|]

poolInfoExpected :: PoolInfo
poolInfoExpected =
  PoolInfo
    { _poolInfPoolIdBech32 = "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r"
    , _poolInfPoolIdHex = "552d7f65e42fdc4ed602a657b85bd7623a592d21e5dcf7b045132da8"
    , _poolInfActiveEpochNo = EpochNo 290
    , _poolInfVrfKeyHash = "076102096f5fc37a47ff14390f7320c76ea769183d06881d53dc4ea1be51acae"
    , _poolInfMargin = 0
    , _poolInfFixedCost = 340000000
    , _poolInfPledge = 10000000000
    , _poolInfRewardAddr = Just "stake1uxtdm5q9j89mmme0l6jt9rsap8cn3vjy3eflptx34s2645ge7r9gp"
    , _poolInfOwners  =
      [
        "stake1uxtdm5q9j89mmme0l6jt9rsap8cn3vjy3eflptx34s2645ge7r9gp"
      ]
    , _poolInfRelays =
      [
        Relay
          { _relayDns = Nothing
          , _relaySrv = Nothing
          , _relayIpv4 = Just "202.182.106.104"
          , _relayIpv6 = Nothing
          , _relayPort = Just 6000
          }
      ]
    , _poolInfMetaUrl = Just "https://git.io/J0eoF"
    , _poolInfMetaHash = Just "63c1b74c5489659d8ec374fa9f22f3e185c5ac458ecc69fdc324ba3bbe2c5093"
    , _poolInfMetaJson  =
        Just $ PoolMetaJson
          { _poolMetaJsonName = "CHOCO Stake Pool"
          , _poolMetaJsonTicker = Just "CHOCO"
          , _poolMetaJsonHomepage = Just "https://twitter.com/choco_stake"
          , _poolMetaJsonDescription = Just "Choco pool description"
          }
    , _poolInfPoolStatus = Just "registered"
    , _poolInfRetiringEpoch = Nothing
    , _poolInfOpCert = Just "cdac991e5a547c86fe869b37eb58c77cc3804d2e592200c835da0b3dbfe8e763"
    , _poolInfOpCertCounter = Just 7
    , _poolInfActiveStake = Just 3847126072870
    , _poolInfSigma = Just "0.00015093541821247124"
    , _poolInfBlockCount  = Just 307
    , _poolInfLivePledge = Just 38784976226
    , _poolInfLiveStake = Just 3848841013319
    , _poolInfLiveDelegators = 24
    , _poolInfLiveSaturation = Just "5.4200"
    }


poolMetadataSample :: ByteString
poolMetadataSample = [r|
  {
    "pool_id_bech32": "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r",
    "meta_url": "https://git.io/J0eoF",
    "meta_hash": "63c1b74c5489659d8ec374fa9f22f3e185c5ac458ecc69fdc324ba3bbe2c5093",
    "meta_json": {
      "name": "CHOCO Stake Pool",
      "ticker": "CHOCO",
      "homepage": "https://twitter.com/choco_stake",
      "description": "Choco pool description"
    }
  }
|]

poolMetadataExpected :: PoolMetadata
poolMetadataExpected =
  PoolMetadata
    { _poolMetadataPoolIdBech32 = "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r"
    , _poolMetadataMetaUrl  = Just "https://git.io/J0eoF"
    , _poolMetadataMetaHash = Just "63c1b74c5489659d8ec374fa9f22f3e185c5ac458ecc69fdc324ba3bbe2c5093"
    , _poolMetadataMetaJson =
        Just $ PoolMetaJson
          { _poolMetaJsonName = "CHOCO Stake Pool"
          , _poolMetaJsonTicker = Just "CHOCO"
          , _poolMetaJsonHomepage = Just "https://twitter.com/choco_stake"
          , _poolMetaJsonDescription = Just "Choco pool description"
          }
    }


poolRelaySample :: ByteString
poolRelaySample = [r|
  [
    {
      "pool_id_bech32": "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r",
      "relays": [
        {
          "dns": null,
          "srv": null,
          "ipv4": "202.182.106.104",
          "ipv6": null,
          "port": 6000
        }
      ]
    }
  ]
|]

poolRelayExpected :: [PoolRelay]
poolRelayExpected =
  [
    PoolRelay
      { _poolRelPoolIdBech32 = "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r"
      , _poolRelRelays  =
        [
          Relay
            { _relayDns = Nothing
            , _relaySrv = Nothing
            , _relayIpv4 = Just "202.182.106.104"
            , _relayIpv6 = Nothing
            , _relayPort = Just 6000
            }
        ]
      }
  ]


poolUpdatesSample :: ByteString
poolUpdatesSample = [r|
  [
    {
      "tx_hash": "58011f2d795af54ab076320d5092a7989efd451eb39c5c64794c7b5eccd9da97",
      "block_time": 1629034536,
      "pool_id_bech32": "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r",
      "pool_id_hex": "552d7f65e42fdc4ed602a657b85bd7623a592d21e5dcf7b045132da8",
      "active_epoch_no": 286,
      "vrf_key_hash": "076102096f5fc37a47ff14390f7320c76ea769183d06881d53dc4ea1be51acae",
      "margin": 0,
      "fixed_cost": 340000000,
      "pledge": 10000000000,
      "reward_addr": "stake1uxtdm5q9j89mmme0l6jt9rsap8cn3vjy3eflptx34s2645ge7r9gp",
      "owners": [
        "stake1uxtdm5q9j89mmme0l6jt9rsap8cn3vjy3eflptx34s2645ge7r9gp"
      ],
      "relays": [
        {
          "dns": null,
          "srv": null,
          "ipv4": "139.180.198.13",
          "ipv6": null,
          "port": 6000
        }
      ],
      "meta_url": "https://git.io/J0eoF",
      "meta_hash": "563af2ed89859d9eb1976706f86285f7b5c8f51a3fa354b185b8f86797416b97",
      "meta_json": null,
      "pool_status": "registered",
      "retiring_epoch": null
    }
  ]
|]

poolUpdatesExpected :: [PoolUpdate]
poolUpdatesExpected =
  [
    PoolUpdate
      { _poolUpdateTxHash = "58011f2d795af54ab076320d5092a7989efd451eb39c5c64794c7b5eccd9da97"
      , _poolUpdateBlockTime  = Just 1629034536
      , _poolUpdatePoolIdBech32 = "pool125kh7e0y9lwya4sz5etmsk7hvga9jtfpuhw00vz9zvk6sh8xh5r"
      , _poolUpdatePoolIdHex = "552d7f65e42fdc4ed602a657b85bd7623a592d21e5dcf7b045132da8"
      , _poolUpdateActiveEpochNo = EpochNo 286
      , _poolUpdateVrfKeyHash = "076102096f5fc37a47ff14390f7320c76ea769183d06881d53dc4ea1be51acae"
      , _poolUpdateMargin = 0
      , _poolUpdateFixedCost = 340000000
      , _poolUpdatePledge = 10000000000
      , _poolUpdateRewardAddr = Just "stake1uxtdm5q9j89mmme0l6jt9rsap8cn3vjy3eflptx34s2645ge7r9gp"
      , _poolUpdateOwners = ["stake1uxtdm5q9j89mmme0l6jt9rsap8cn3vjy3eflptx34s2645ge7r9gp"]
      , _poolUpdateRelays =
          [
            Relay
            { _relayDns = Nothing
            , _relaySrv = Nothing
            , _relayIpv4 = Just "139.180.198.13"
            , _relayIpv6 = Nothing
            , _relayPort = Just 6000
            }
          ]
      , _poolUpdateMetaUrl = Just "https://git.io/J0eoF"
      , _poolUpdateMetaHash = Just "563af2ed89859d9eb1976706f86285f7b5c8f51a3fa354b185b8f86797416b97"
      , _poolUpdateMetaJson = Nothing
      , _poolUpdatePoolStatus = Just "registered"
      , _poolUpdateRetiringEpoch  = Nothing
      }
  ]
