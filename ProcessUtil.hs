{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module ProcessUtil where

import Data.ByteString (ByteString)
import System.IO
import System.Posix.Process.ByteString
import System.Exit
import Data.List ((\\))
import Data.Maybe (catMaybes)
import GHC.Conc (threadWaitRead)
import System.Posix.IO.ByteString
import Control.Monad.State
import System.Posix.Types
import System.Posix.ByteString.FilePath
import System.Posix.Signals
import System.Posix.Env.ByteString (getEnvDefault)

import Common

-- from tcp_serve
getShell :: (MonadIO m) => m ByteString
getShell = liftIO $ getEnvDefault "SHELL" "/bin/sh"

executeInForkShell :: (MonadState [Fd] m, MonadIO m) =>
                      Bool ->
                      ByteString ->
                      Maybe [(ByteString, ByteString)] ->
                      Maybe Fd -> -- stdin
                      Maybe Fd -> -- stdout
                      m ProcessID
executeInForkShell waitRead cmd env stdin stdout = do
  sh <- getShell
  pid <- executeInFork waitRead sh False ["-c", cmd] env stdin stdout
  debug $ putStrLn $ "forked " ++ show pid ++ " " ++ show cmd ++ " WaitForInput? " ++ show waitRead
  return pid
  
-- from hssh
executeInFork :: (MonadState [Fd] m, MonadIO m) =>
                 Bool ->
                 RawFilePath -> Bool -> [ByteString] ->
                 Maybe [(ByteString, ByteString)] ->
                 Maybe Fd -> -- stdin
                 Maybe Fd -> -- stdout
                 m ProcessID
executeInFork waitRead file searchPath args env stdin stdout = do
  closeFds <- get
  liftIO $ forkProcess $ do
    case stdin of
      Nothing -> return ()
      Just fd -> dupTo fd stdInput >> return ()
    case stdout of
      Nothing -> return ()
      Just fd -> do dupTo fd stdOutput >> return ()
    mapM_ closeFd (closeFds \\ catMaybes [stdin, stdout])
    when waitRead $ threadWaitRead stdInput
    executeFile file searchPath args env

testAndCollect prog = do
  (pids, closeFds) <- runTest prog
  mapM_ closeFd closeFds
  collectChildrenWait pids

runTest prog = do
--  installHandler sigCHLD (Catch collectAnyChild) Nothing
  flip runStateT [] prog

-- from tcp_serve
collectAnyChild = do
    res <- getAnyProcessStatus False True
    case res of
      Nothing -> return ()
      Just (pid, pStatus) -> do
        case pStatus of
          Exited ecode ->
            if ecode /= ExitSuccess
            then hPutStrLn stderr $ show pid ++ " exited with " ++ show pStatus
            else return ()
          Terminated _ True  -> hPutStrLn stderr $ show pid ++ " crashed with core dump"
          Terminated s False -> hPutStrLn stderr $ show pid ++ " terminated through signal " ++ show s
          Stopped s          -> hPutStrLn stderr $ show pid ++ " stopped through signal " ++ show s

collectChildrenWait [] = return ()
collectChildrenWait pids = do  
    res <- getAnyProcessStatus True True
    case res of
      Nothing -> error $ "No child waiting to be reaped, but I was waiting for " ++ show pids
      Just (pid, pStatus) -> do
        case pStatus of
          Exited ecode ->
            if ecode /= ExitSuccess
            then hPutStrLn stderr $ show pid ++ " exited with " ++ show pStatus
            else return ()
          Terminated _ True  -> hPutStrLn stderr $ show pid ++ " crashed with core dump"
          Terminated s False -> hPutStrLn stderr $ show pid ++ " terminated through signal " ++ show s
          Stopped s          -> hPutStrLn stderr $ show pid ++ " stopped through signal " ++ show s
        collectChildrenWait (pids \\ [pid])
