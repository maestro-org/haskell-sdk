module Maestro.Run.General where

import           Maestro.Client
import           Text.Printf          (printf)

runGeneralAPI :: MaestroEnv -> IO ()
runGeneralAPI mEnv = do
  chainTip <- getChainTip mEnv 
  printf "Querying chain-tip, received: ⮯\n%s\n" (show chainTip)
