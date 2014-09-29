{-# LANGUAGE
    GADTs,
    ExistentialQuantification,
    OverloadedStrings,
    FlexibleContexts,
    NoMonomorphismRestriction,
    CPP #-}

module Tcp_Arrow where

import Network
import System.IO
import Data.ByteString (ByteString)
import System.Posix.Types
import System.Posix.IO.ByteString
import System.Posix.Process.ByteString
import System.Posix.ByteString.FilePath
import System.Posix.Signals
import System.Exit
import qualified Data.ByteString as B
import Pipes
import System.Posix.Env.ByteString (getEnvDefault)
import Data.List ((\\))
import Data.Maybe (catMaybes)
import Control.Monad.State
import GHC.Conc (threadWaitRead)

data EP_Handle
data EP_Pure a
data EP_Void

data Process a b m r where
  SimpleProcess     :: (MonadIO m) => ByteString -> Bool -> Process EP_Handle EP_Handle m ()
  SourceProcess     :: (MonadIO m) => ByteString ->         Process EP_Void EP_Handle m ()
  SinkProcess       :: (MonadIO m) => ByteString -> Bool -> Process EP_Handle EP_Void m ()  
--  ControlledProcess :: (MonadIO m) => String -> Process EP_Handle EP_Handle m ()
  Pure              :: Pipe a b m r -> Process (EP_Pure a) (EP_Pure b) m r

data Network a b m r where
  SingleProcess :: Process a b m r -> Network a b m r
  Handle_Handle :: Network a EP_Handle m r -> Network EP_Handle b m r -> Network a b m r
  Handle_Pure   :: Network a EP_Handle m r -> Network (EP_Pure ByteString) b m r -> Network a b m r
  Pure_Handle   :: Network a (EP_Pure ByteString) m r -> Network EP_Handle b m r -> Network a b m r
  Pure_Pure     :: Network a (EP_Pure c) m r -> Network (EP_Pure c) b m r -> Network a b m r

type Input b m r = Process EP_Void b m r
type Output a m r = Process a EP_Void m r                   

--runNetwork :: (MonadIO m) => Network EP_Void EP_Void m r -> m r
--runNetwork = (Serial n1 c n2)

saveCloseFds :: (MonadState [Fd] m) => [Fd] -> m ()
saveCloseFds fds = get >>= put . (fds ++)
removeCloseFds :: (MonadState [Fd] m) => [Fd] -> m ()
removeCloseFds fds = get >>= put . (\\ fds)

singleton a = [a]

--runSimpleProcess :: (MonadIO m) => SimpleProcess EP_Handle EP_Handle m r -> Fd -> Fd -> m ()
runHandleHandle :: (MonadIO m, MonadState [Fd] m) => Network EP_Handle EP_Handle m r -> Fd -> Fd -> m [CPid]
runHandleHandle (SingleProcess (SimpleProcess cmd waitRead)) fdIn fdOut = do
  debug $ putStrLn $ "runHandleHandle: execute process using fds " ++ show fdIn ++ ", " ++ show fdOut
  executeInForkShell waitRead cmd Nothing (Just fdIn) (Just fdOut) >>= return . singleton
runHandleHandle (Handle_Handle p1 p2) fdIn fdOut = do
  (intermRead, intermWrite) <- liftIO createPipe
  debug $ putStrLn $ "runHandleHandle: created fds " ++ show intermRead ++ ", " ++ show intermWrite
  saveCloseFds [intermRead, intermWrite]    
  pids1 <- runHandleHandle p1 fdIn intermWrite
  pids2 <- runHandleHandle p2 intermRead fdOut
  liftIO $ mapM_ closeFd [intermRead, intermWrite]
  debug $ putStrLn $ "runHandleHandle: close fds " ++ show intermRead ++ ", " ++ show intermWrite
  removeCloseFds [intermRead, intermWrite]    
  return (pids1 ++ pids2)

runVoidHandle :: (MonadIO m, MonadState [Fd] m) => Network EP_Void EP_Handle m r -> Fd -> m [CPid]
runVoidHandle (SingleProcess (SourceProcess cmd)) fdOut = do
  debug $ putStrLn $ "runVoidHandle: execute process using fd " ++ show fdOut  
  executeInForkShell False cmd Nothing Nothing (Just fdOut)  >>= return . singleton

runHandleVoid :: (MonadIO m, MonadState [Fd] m) => Network EP_Handle EP_Void m r -> Fd -> m [CPid]
runHandleVoid (SingleProcess (SinkProcess cmd waitRead)) fdIn = do
  debug $ putStrLn $ "runHandleVoid: execute process using fd " ++ show fdIn
  executeInForkShell waitRead cmd Nothing (Just fdIn) Nothing >>= return . singleton
runHandleVoid (Handle_Handle p1 p2) fdIn = do
  (intermRead, intermWrite) <- liftIO createPipe
  saveCloseFds [intermRead, intermWrite]
  debug $ putStrLn $ "runHandleVoid: created fds " ++ show intermRead ++ ", " ++ show intermWrite
  pids1 <- runHandleHandle p1 fdIn intermWrite
  pids2 <- runHandleVoid p2 intermRead
  liftIO $ mapM_ closeFd [intermRead, intermWrite]
  debug $ putStrLn $ "runHandleVoid: close fds " ++ show intermRead ++ ", " ++ show intermWrite
  removeCloseFds [intermRead, intermWrite]  
  return (pids1 ++ pids2)
  
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

ls = SingleProcess (SourceProcess "ls")
ls' = SingleProcess (SimpleProcess "ls" False)
mkUpper = SingleProcess (SimpleProcess "tr [a-z] [A-Z]" True)
idProc = SingleProcess (SimpleProcess "cat" True)
testWithErr = SingleProcess (SimpleProcess "./test" True)
longRunning = SingleProcess (SimpleProcess "./test2" True)
save fp = SingleProcess (SinkProcess ("cat > " `B.append` fp) True)

t1 = Handle_Handle ls' (save "foo")

t2 = Handle_Handle
     (Handle_Handle ls' mkUpper)
     (save "foo")

t3 = Handle_Handle
     (Handle_Handle ls' idProc)
     (save "foo")

t4 = Handle_Handle
     (Handle_Handle ls' testWithErr)
     (save "foo")

t5 = Handle_Handle
     (Handle_Handle ls' longRunning)
     (save "foo")

runTest prog = do
--  installHandler sigCHLD (Catch collectAnyChild) Nothing
  flip runStateT [] prog

testAndCollect prog = do
  (pids, closeFds) <- runTest prog
  mapM_ closeFd closeFds
  collectChildrenWait pids
  
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

-- debug utility
debug :: (MonadIO m) => IO () -> m ()
#ifdef DEBUG
debug f = liftIO f
#else
debug _ = return ()
#endif
