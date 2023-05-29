module Maestro.Run.Pools where

import Maestro.Client.Env
import Maestro.Client.Pools
import Maestro.Types

poolId :: Bech32StringOf PoolId
poolId = "pool1rkfs9glmfva3jd0q9vnlqvuhnrflpzj4l07u6sayfx5k7d788us"

runPoolsAPI :: MaestroEnv -> IO ()
runPoolsAPI mEnv = do
  putStrLn "Fetching List Pools ..."
  lstPools <- runListPools mEnv
  putStrLn $ "fetched List Pools: \n " ++ show lstPools

  putStrLn "Fetching Pool Blocks ..."
  blocks <- runPoolBlocks mEnv
  putStrLn $ "fetched Pool Blocks: \n " ++ show blocks

  putStrLn "Fetching Pool Delegators ..."
  delegators <- runPoolBlocks mEnv
  putStrLn $ "fetched pool Delegators: \n " ++ show delegators

  putStrLn "Fetching Pool History ..."
  hist <- runPoolBlocks mEnv
  putStrLn $ "fetched pool History: \n " ++ show hist

  putStrLn "Fetching Pool Info ..."
  info <- runPoolInfo mEnv
  putStrLn $ "fetched pool Info: \n " ++ show info

  putStrLn "Fetching Pool Metadata ..."
  metadata <- runPoolInfo mEnv
  putStrLn $ "fetched pool Metadata: \n " ++ show metadata

  putStrLn "Fetching Pool Relays ..."
  relays <- runPoolInfo mEnv
  putStrLn $ "fetched pool Relays: \n " ++ show relays

  putStrLn "Fetching Pool Updates ..."
  updates <- runPoolInfo mEnv
  putStrLn $ "fetched pool Updates: \n " ++ show updates

runPoolUpdates :: MaestroEnv -> IO [PoolUpdates]
runPoolUpdates mEnv = poolUpdates mEnv poolId

runListPools :: MaestroEnv -> IO [Pool]
runListPools mEnv = listPools mEnv (Page 1 1)

runPoolBlocks :: MaestroEnv -> IO [PoolBlock]
runPoolBlocks mEnv = poolBlocks mEnv poolId (Page 1 1) Nothing (Just Ascending)

runPoolDelegators :: MaestroEnv -> IO [DelegatorInfo]
runPoolDelegators mEnv = poolDelegators mEnv poolId (Page 1 1)

runPoolHistory :: MaestroEnv -> IO [PoolHistory]
runPoolHistory mEnv = poolHistory mEnv poolId (Page 1 1) Nothing (Just Ascending)

runPoolInfo :: MaestroEnv -> IO PoolInfo
runPoolInfo mEnv = poolInfo mEnv poolId

runPoolMetadata :: MaestroEnv -> IO PoolMetadata
runPoolMetadata mEnv = poolMetadata mEnv poolId

runPoolRelay :: MaestroEnv -> IO [PoolRelay]
runPoolRelay mEnv = poolRelays mEnv poolId
