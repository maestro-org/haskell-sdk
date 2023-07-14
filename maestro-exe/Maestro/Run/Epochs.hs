module Maestro.Run.Epochs where

import           Maestro.Client.V0

runEpochsAPI :: MaestroEnv 'V0 -> IO ()
runEpochsAPI mEnv = do
  putStrLn "Fetching Current Epoch's Info ..."
  currentEpochInfo <- getCurrentEpoch mEnv
  putStrLn $ "Received: ⮯\n" ++ show currentEpochInfo
  putStrLn "Fetching 70th Epoch's Info ..."
  epochInfo <- getEpochInfo mEnv 70
  putStrLn $ "Received: ⮯\n" ++ show epochInfo
