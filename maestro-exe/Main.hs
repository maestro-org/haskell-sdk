module Main (main) where

import qualified Data.Text           as T
import           Maestro.Client.Env
import           Maestro.Run.Datum
import           Maestro.Run.Epochs
import           Maestro.Run.General
import           Maestro.Run.Pools
import           Maestro.Run.Scripts
import           Maestro.Run.Tx
import           Maestro.Run.AddressV1
import           System.Environment  (getEnv)


main :: IO ()

main = do
    apiKey <- maestroKey
    apiKeyMain <- maestroMainKey
    env <- mkMaestroEnv (T.pack apiKey) Preprod V0
    runPoolsAPI env
    runTxApi env
    runEpochsAPI env
    runDatumAPI env
    runScriptsAPI env
    runGeneralAPI env
    env' <- mkMaestroEnv (T.pack apiKeyMain) Mainnet V1
    runV1AddressAPI env'

    where
      maestroKey = getEnv  "MAESTRO_API_KEY"
      maestroMainKey = getEnv "MAESTRO_MAIN_KEY"
