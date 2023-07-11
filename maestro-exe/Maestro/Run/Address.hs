module Maestro.Run.Address where

import           Control.Monad           (unless)
import           Data.List               (sort)
import qualified Data.Text               as T (pack)
import           Maestro.Client.Env
import qualified Maestro.Client.V0       as V0
import qualified Maestro.Client.V1       as V1
import           Maestro.Types.V1.Common (v1UtxoWithSlotToV0)

runAddressAPI :: String -> IO ()
runAddressAPI apiKey = do
  mEnvV0 <- mkMaestroEnv @'V0 (T.pack apiKey) Preprod
  mEnvV1 <- mkMaestroEnv @'V1 (T.pack apiKey) Preprod
  let addrs = undefined  -- Mention list of addresses.
  utxos <- V0.allPages $ flip (V0.utxosAtMultiAddresses mEnvV0 Nothing Nothing) addrs
  let utxosSorted = sort utxos
  utxos' <- fmap (fmap v1UtxoWithSlotToV0) $ V1.allPages $ flip (V1.utxosAtMultiAddresses mEnvV1 Nothing Nothing) addrs
  let utxos'Sorted = sort utxos'
  unless (utxosSorted == utxos'Sorted) $ error "Not same"
