module Maestro.Run.Tx where

import           Maestro.Client.V0
import           Maestro.Types.V0

txHash :: HashStringOf Tx
txHash = "7fdf7a20ba50d841344ab0cb368da6a047ce1e2a29b707586f61f0b8fea6bcf2"

runTxApi :: MaestroEnv 'V0 -> IO ()
runTxApi mEnv = do
  putStrLn "Fetching Tx Address ..."
  txAddr <- runTxAddress mEnv
  putStrLn $ "fetched Tx Addr: \n " ++ show txAddr

  putStrLn "Fetching Tx Cbor ..."
  cbor <- runTxCbor mEnv
  putStrLn $ "fetched Tx Cbor: \n " ++ show cbor

  putStrLn "Fetching Tx Utxo"
  utxo <- runTxUtxo mEnv
  putStrLn $ "fetched Tx Utxos: \n " ++ show utxo

runTxAddress :: MaestroEnv 'V0 -> IO UtxoAddress
runTxAddress mEnv = txAddress mEnv txHash $ TxIndex 0

runTxCbor :: MaestroEnv 'V0 -> IO TxCbor
runTxCbor mEnv = txCbor mEnv txHash

runTxUtxo :: MaestroEnv 'V0 -> IO Utxo
runTxUtxo mEnv = txUtxo mEnv txHash (TxIndex 0) (Just True) (Just True)
