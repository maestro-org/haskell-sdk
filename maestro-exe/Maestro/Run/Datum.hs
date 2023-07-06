module Maestro.Run.Datum where

import           Maestro.Client.V0
import           Text.Printf          (printf)

runDatumAPI :: MaestroEnv 'V0 -> IO ()
runDatumAPI mEnv = do
  let datumHash = "938dc15a5faa3da8e7f1e3ed8ca50b49248f8fffdfc04ff3cf7dffa0d06343eb"  -- Quiet an involved datum.
  printf "Fetching datum from hash %s...\n" datumHash
  datum <- getDatumByHash mEnv datumHash
  putStrLn $ "Received: ⮯\n" ++ show datum
