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
    env <- mkMaestroEnv @'V1 maestroKey Preprod $ Just (50000, 10000000)
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
        Left err -> print err
        Right utxos -> putStrLn $ "OK, UTxO count: " <> show (length utxos)

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


--

-- module Main (main) where

-- import qualified Data.Text           as T
-- import           Maestro.Client.Env
-- -- import           Maestro.Run.Address
-- import           Maestro.Run.Datum
-- import           Maestro.Run.Epochs
-- import           Maestro.Run.General
-- import           Maestro.Run.Pools
-- import           Maestro.Run.Scripts
-- import           Maestro.Run.Tx
-- import           System.Environment  (getEnv)


-- main :: IO ()

-- main = do
--     apiKey <- maestroKey
--     env <- mkMaestroEnv @'V0 (T.pack apiKey) Preprod
--     runPoolsAPI env
--     runTxApi env
--     runEpochsAPI env
--     runDatumAPI env
--     runScriptsAPI env
--     runGeneralAPI env
--     -- runAddressAPI apiKey

--     where
--       maestroKey = getEnv  "MAESTRO_API_KEY"