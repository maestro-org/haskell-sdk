module Maestro.Run.Scripts where

import           Maestro.Client.Env
import           Maestro.Client.Scripts
import           Text.Printf            (printf)

runScriptsAPI :: MaestroEnv -> IO ()
runScriptsAPI mEnv = do
  let scriptHash = "3a888d65f16790950a72daee1f63aa05add6d268434107cfa5b67712"
  printf "Fetching script from hash %s..." scriptHash
  script <- getScriptByHash mEnv scriptHash
  putStrLn $ "Received: ⮯\n" ++ show script
