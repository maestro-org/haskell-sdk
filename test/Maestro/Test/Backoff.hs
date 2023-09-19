module Maestro.Test.Backoff where

import Test.Hspec

import Control.Concurrent (MVar, ThreadId, newMVar, takeMVar, putMVar, newEmptyMVar, forkFinally)
import Control.Exception (try)
import Control.Monad (void)
import Data.Text (pack)
import Maestro.Client.V1
import Maestro.Types.V1
import System.Environment (getEnv)


spec_backoff :: Spec
spec_backoff = do
  it "errors without backoff" $ do
    shouldThrow  (doConcCall Nothing) anyErrorCall

  it "works with default backoff settings" $ do
    doConcCall defaultBackoff

type Ret = Either MaestroError [UtxoWithSlot]

doConcCall :: Maybe (Int, Int) -> IO ()
doConcCall backoffSettings = do
    maestroKey <- pack <$> getEnv  "MAESTRO_API_KEY"
    env <- mkMaestroEnv @'V1 maestroKey Preprod backoffSettings
    children <- newMVar []
    void $ mapM (forkChild children) $ replicate 30 $ task env
    waitForChildren children
  where
    task :: MaestroEnv 'V1 -> IO Ret
    task env =
      try
      $ allPages
      $ flip
        (
          utxosAtMultiAddresses env
            (Just True)
            (Just False)
        ) ["addr_test1vqj247zdmh7n9g46ukk59k2yxeslevzhah0uj3t0t450x3ggycpxj"]

forkChild :: MVar [MVar Ret] -> IO (Ret) -> IO ThreadId
forkChild children action = do
    mvar :: MVar Ret <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkFinally action $ \ret -> case ret of
      Left _ -> putMVar mvar $ Left $ MaestroError "client finished abruptly"
      Right ret' -> putMVar mvar ret'

waitForChildren :: MVar [MVar Ret] -> IO ()
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      ret <- takeMVar m
      case ret of
        Left _ -> error "failed"
        _ -> waitForChildren children


