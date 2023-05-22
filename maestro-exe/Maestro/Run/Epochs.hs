module Maestro.Run.Epochs where

import           Maestro.Client.Env
import           Maestro.Client.Epochs

runEpochsAPI :: MaestroEnv -> IO ()
runEpochsAPI mEnv = do
  putStrLn "Fetching Current Epoch's Info ..."
  currentEpochInfo <- getCurrentEpoch mEnv
  putStrLn $ "Received: ⮯\n" ++ show currentEpochInfo
  putStrLn "Fetching 70th Epoch's Info ..."
  epochInfo <- getEpochInfo mEnv 70
  putStrLn $ "Received: ⮯\n" ++ show epochInfo
