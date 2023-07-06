module Main (main) where

import qualified Data.Text           as T
import           Maestro.Client.Env
import           Maestro.Run.Address
import           Maestro.Run.Datum
import           Maestro.Run.Epochs
import           Maestro.Run.General
import           Maestro.Run.Pools
import           Maestro.Run.Scripts
import           Maestro.Run.Tx
import           System.Environment  (getEnv)


main :: IO ()

main = do
    apiKey <- maestroKey
    apiKeyMain <- maestroMainKey
    env <- mkMaestroEnv @'V0 (T.pack apiKey) Preprod
    runPoolsAPI env
    runTxApi env
    runEpochsAPI env
    runDatumAPI env
    runScriptsAPI env
    runGeneralAPI env
    -- env' <- mkMaestroEnv @'V1 (T.pack apiKeyMain) Mainnet
    env' <- mkMaestroEnv @'V0 (T.pack apiKeyMain) Mainnet
    runAddressAPI env'

    where
      maestroKey = getEnv  "MAESTRO_API_KEY"
      maestroMainKey = getEnv "MAESTRO_MAIN_KEY"
