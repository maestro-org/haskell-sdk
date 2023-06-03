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
2. Create a [Maestro API key](https://docs.gomaestro.org/docs/Getting-started/Sign-up-login).
3. Create environment for accessing the API:
```haskell
import Maestro.Client.Env

myEnvPreprod <- mkMaestroEnv "Your-API-Key" Preprod
myEnvMainnet <- mkMaestroEnv "Your-API-Key" Mainnet
```
4. Example: chain tip
```haskell
getChainTip myEnvPreprod  -- Preprod
getChainTip myEnvMainnet  -- Mainnet
```

Other endpoints in the `General` category can be exmained in the [`Maestro.Client.General`](https://haddock.gomaestro.org/Maestro-Client-General.html) Haddock module.

# Documentation

* [SDK Haddock](https://haddock.gomaestro.org/)
* [Maestro public docs](https://docs.gomaestro.org/)
* [Maestro API reference](https://reference.gomaestro.org/)

# Contributing

Meastro welcomes all contributors! Please see our [contributing guidelines](CONTRIBUTING.md) and [code of conduct](CODE_OF_CONDUCT.md).
