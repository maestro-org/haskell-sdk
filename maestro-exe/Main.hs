{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Concurrent (MVar, ThreadId, newMVar, takeMVar, putMVar, newEmptyMVar, forkFinally)
import Control.Exception (try)
import Maestro.Client.V1
import Maestro.Types.V1
import System.Environment (getEnv)
import Data.Text qualified as T
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)


main :: IO ()
main = do
    maestroKey <- T.pack <$> getEnv  "MAESTRO_API_KEY"
    env <- mkMaestroEnv @'V1 maestroKey Preprod 50000
    void $ mapM forkChild $ replicate 30 $ task env
    waitForChildren
  where
    task env = do
      addressesUTxOs :: Either MaestroError [UtxoWithSlot] <-
        try
        $ allPages
        $ flip
          (
            utxosAtMultiAddresses env
              (Just True)
              (Just False)
          ) ["addr_test1vqj247zdmh7n9g46ukk59k2yxeslevzhah0uj3t0t450x3ggycpxj"]
      case addressesUTxOs of
        Left err -> print  err
        Right utxos -> print $ length utxos

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkFinally io (\_ -> putMVar mvar ())