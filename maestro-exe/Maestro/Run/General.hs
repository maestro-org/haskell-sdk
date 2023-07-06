module Maestro.Run.General where

import           Maestro.Client.V0
import           Text.Printf          (printf)

runGeneralAPI :: MaestroEnv 'V0 -> IO ()
runGeneralAPI mEnv = do
  chainTip <- getChainTip mEnv 
  printf "Querying chain-tip, received: ⮯\n%s\n" (show chainTip)
