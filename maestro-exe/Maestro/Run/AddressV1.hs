module Maestro.Run.AddressV1 where

import           Maestro.Client.V1

runV1AddressAPI :: MaestroEnv -> IO ()
runV1AddressAPI mEnv = do
  utxos <- allPages $ (flip $ utxosAtMultiAddresses mEnv Nothing Nothing) ["insert","your", "big", "address", "list", "here"]
  putStrLn $ "Received: ⮯\n" ++ show utxos
  -- writeFile "allUtxos.txt" $ show utxos
