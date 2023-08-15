module Maestro.Run.Tx where

import           Maestro.Client.V0
import           Maestro.Types.V0

import           Maestro.Client.V1
import           Maestro.Types.V1

txHash :: HashStringOf Tx
txHash = "3cc22b391e63d0ebf1b0553dc100253b8cba3e715e95672319c7e6e5cb6739e9"

runTxApiV1 :: MaestroEnv 'V1 -> IO ()
runTxApiV1 mEnv = do
    putStrLn "Fetching Tx Details"
    txDetails <- txDetailsByHash mEnv txHash
    putStrLn $ "fetched Tx Details: \n " ++ show txDetails
    putStrLn "Fetching Block Details"
    blockDetails <- blockDetailsByHash mEnv
                    $ _txDetailsBlockHash $ getTimestampedData txDetails
    putStrLn $ "fetched Block Details: \n " ++ show blockDetails

runTxApiV0 :: MaestroEnv 'V0 -> IO ()
runTxApiV0 mEnv = do
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
