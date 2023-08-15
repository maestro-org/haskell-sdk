module Main (main) where

import qualified Data.Text           as T
import           Maestro.Client.Env
--import           Maestro.Run.Address
import           Maestro.Run.Datum
import           Maestro.Run.Epochs
import           Maestro.Run.General
import           Maestro.Run.Pools
import           Maestro.Run.Scripts
import           Maestro.Run.Tx
import           System.Environment  (getEnv)


main :: IO ()
main = do
    apiV0Key <- maestroV0Key
    apiV1Key <- maestroV1Key
    envV0 <- mkMaestroEnv @'V0 (T.pack apiV0Key) Preprod
    envV1 <- mkMaestroEnv @'V1 (T.pack apiV1Key) Preprod
    runTxApiV1 envV1
    runTxApiV0 envV0
    runPoolsAPI envV0
    runEpochsAPI envV0
    runDatumAPI envV0
    runScriptsAPI envV0
    runGeneralAPI envV0
    --runAddressAPI apiV0Key
  where
    maestroV0Key = getEnv  "MAESTRO_API_V0_KEY"
    maestroV1Key = getEnv  "MAESTRO_API_V1_KEY"
