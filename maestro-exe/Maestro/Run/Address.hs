module Maestro.Run.Address where

import           Control.Monad           (when)
import           Data.List               (sort)
import           Maestro.Client.Env
import qualified Maestro.Client.V0       as V0
import qualified Maestro.Client.V1       as V1
import           Maestro.Types.V1.Common (v1UtxoToV0)

runAddressAPI :: MaestroEnv 'V0 -> MaestroEnv 'V1 -> IO ()
runAddressAPI mEnvV0 mEnvV1 = do
  let addrs = undefined  -- Mention list of addresses.
  utxos <- V0.allPages $ flip (V0.utxosAtMultiAddresses mEnvV0 Nothing Nothing) addrs
  let utxosSorted = sort utxos
  utxos' <- fmap (fmap v1UtxoToV0) $ V1.allPages $ flip (V1.utxosAtMultiAddresses mEnvV1 Nothing Nothing) addrs
  let utxos'Sorted = sort utxos'
  when (utxosSorted == utxos'Sorted) $ putStrLn "Yes"
