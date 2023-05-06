<p align="center">
  <a href="https://www.gomaestro.org/">
    <img src="https://www.gomaestro.org/logos/LandingLogos/DarkLogo.svg" alt="Maestro Logo" width="425" />
  </a>
  <h2 align="center">Haskell SDK for <a href="https://www.gomaestro.org/">Maestro</a> API</h2>
  <p align="center">
    <a href="TODO">
      <img src="https://img.shields.io/badge/-Haddock-5E5184?style=flat-square&logo=haskell&logoColor=white" />
    </a>
    <a href="https://docs.gomaestro.org/docs/intro">
      <img src="https://img.shields.io/badge/-Documentation-blue?style=flat-square&logo=semantic-scholar&logoColor=white" />
    </a>
    <a href="https://github.com/maestro-org/haskell-sdk/blob/main/LICENSE">
      <img src="https://img.shields.io/github/license/maestro-org/haskell-sdk?style=flat-square&label=Licence" />
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
2. Get API token for your project created at [gomaestro.org](https://www.gomaestro.org/).
3. Create environment, against which we'll run our API endpoints:

    ```haskell
    import Maestro.Client.Env

    myEnvPreprod <- mkMaestroEnv "Your-API-Key" Preprod
    myEnvMainnet <- mkMaestroEnv "Your-API-Key" Mainnet
    ```

4. Now say we want to query endpoints belonging to _"General"_ category with respect to https://reference.gomaestro.org/, then looking at the haddock of exported interface for [`Maestro.Client.General`](TODO) module, say we are interested in endpoint to get for chain tip, we can obtain detailed information of the resulting type by seeing haddock of [`ChainTip`](TODO) type and query for it as follows: 

    ```haskell
    getChainTip myEnvPreprod  -- To get for Preprod network.
    getChainTip myEnvMainnet  -- To get for Mainnet network.
    ```

  And that's it!

# Documentation

See haddock for this package [here](TODO) and for documentation for Maestro is available at [docs.gomaestro.org](https://docs.gomaestro.org/).

# Contributing

Meastro welcomes all contributors! Please see our [contributing guidelines](CONTRIBUTING.md) and [code of conduct](CODE_OF_CONDUCT.md).
