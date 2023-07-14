<p align="center">
  <a href="https://www.gomaestro.org/">
    <img src="https://www.gomaestro.org/logos/LandingLogos/DarkLogo.svg" alt="Maestro Logo" width="425" />
  </a>
  <h2 align="center">Haskell SDK for the <a href="https://www.gomaestro.org/">Maestro</a> Dapp Platform</h2>
  <p align="center">
    <a href="https://haddock.gomaestro.org/">
      <img src="https://img.shields.io/badge/-Haddock-5E5184?style=flat-square&logo=haskell&logoColor=white" />
    </a>
    <a href="https://docs.gomaestro.org/docs/intro">
      <img src="https://img.shields.io/badge/-Docs-blue?style=flat-square&logo=semantic-scholar&logoColor=white" />
    </a>
    <a href="https://github.com/maestro-org/haskell-sdk/blob/main/LICENSE">
      <img src="https://img.shields.io/github/license/maestro-org/haskell-sdk?style=flat-square&label=License" />
    </a>
    <a href="https://github.com/maestro-org/haskell-sdk/actions/workflows/build.yml?query=branch%3Amain">
      <img src="https://img.shields.io/github/actions/workflow/status/maestro-org/haskell-sdk/build.yml?style=flat-square&branch=main&label=Build" />
    </a>
    <a href="./CONTRIBUTING.md">
      <img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square" />
    </a>
    <a href="https://twitter.com/GoMaestroOrg">
      <img src="https://img.shields.io/badge/-%40GoMaestroOrg-F3F1EF?style=flat-square&logo=twitter&logoColor=1D9BF0" />
    </a>
    <a href="https://discord.gg/ES2rDhBJt3">
      <img src="https://img.shields.io/badge/-Discord-414EEC?style=flat-square&logo=discord&logoColor=white" />
    </a>
  </p>
</p>

# Getting Started

1. Add `maestro-sdk` to the `build-depends` of your project.
2. Create a [Maestro API key](https://docs.gomaestro.org/docs/Getting-started/Sign-up-login).
3. Code below explains sample usage.
  ```haskell
  module Main (main) where

  import           Control.Exception (try)
  import           Maestro.Client.V1  -- @Maestro.Client.V1@ defines all the client utilities to query Maestro API endpoints.
  import           Maestro.Types.V1  -- @Maestro.Types.V1@ defines all the types used.

  main :: IO ()
  main = do
    env <- mkMaestroEnv @'V1 "<Your-API-Key>" Preprod  -- This is how we create an environment against which we'll query endpoints.
    chainTip :: ChainTip <- getTimestampedData <$> getChainTip env  -- Maestro endpoint to get for chain-tip has data & timestamp against which data was calculated. All endpoints which are timestamped, has functions `getTimestampedData` to get for underlying data & `getTimestamp` to get the timestamp.
    addressesUTxOs :: Either MaestroError [UtxoWithSlot] <-
      try  -- To catch for any errors, given in type `MaestroError`.
      $ allPages  -- Since this endpoint is paged, we have a helper utility `allPages` to accumulate data from all the pages.
      $ flip
        (
          utxosAtMultiAddresses env
            (Just True)  -- We would like to have datums resolved. This is for @resolve_datums@ query parameter.
            (Just False)  -- We would not like to include CBOR encodings of the transaction outputs in the response.
        ) ["addr_test1...", "addr_test1...", "addr_test1..."]  -- Mention your list of addresses to query for.
    print addressesUTxOs
  ```

# Documentation

* [SDK Haddock](https://haddock.gomaestro.org/)
* [Maestro public docs](https://docs.gomaestro.org/)
* [Maestro API reference](https://docs.gomaestro.org/docs/category/rest-api-reference)

# Contributing

Meastro welcomes all contributors! Please see our [contributing guidelines](CONTRIBUTING.md) and [code of conduct](CODE_OF_CONDUCT.md).
