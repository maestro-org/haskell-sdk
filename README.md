# haskell-sdk

## Setup 

Get Maestro api key from (https://dashboard.gomaestro.org/login)

Create Maestro environment  & call client APIs
   
```haskell
     import Maestro.Client.Env
     
     mEnv <- mkMaestroEnv "api-key"  Preprod
     
     -- we can use maestro environment to access Client Apis (Eg.)
     
     accountInfo <- Maestro.Client.Accounts.getAccount mEnv "stake_......." 
  
```
