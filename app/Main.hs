module Main where

import LoadCK2
import System.Directory
import Generator
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Environment
import Types
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import GHC.Conc.Sync

modifiedOnly :: Event -> Bool
modifiedOnly (Modified _ _ _) = True
modifiedOnly _ = False

loop :: IO a -> IO ()
loop l = do
  l
  loop l

printError :: SomeException -> IO ()
printError e = do
  print e
  putStrLn "exporting failed"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "give main CK2 directory as a first argument"
      return ()
    mainCK2Dir:_ -> do
      ls0 <- initialLoaderState mainCK2Dir
      putStrLn "initial state loaded"
      mvar <- newTMVarIO ()
      -- watcher
      forkIO (withManager $ \mgr -> do
        putStrLn $ "watching directory " ++ (lsSaveDir ls0)
        watchDir mgr (lsSaveDir ls0) modifiedOnly
          (\e -> do
            putStrLn "noticed change..."
            atomically (tryPutTMVar mvar ())
            return ())
        forever $ threadDelay 1000000)
      loop (do
        atomically (readTMVar mvar)
        putStrLn "exporting started"
        catch (do
          (loadSaveGame ls0) >>= generate
          putStrLn "export successful") printError
        atomically (takeTMVar mvar)
        )
