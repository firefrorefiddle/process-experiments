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
import Pipes.ByteString (fromHandle, toHandle)
import System.Posix.Env.ByteString (getEnvDefault)
import Data.List ((\\))
import Data.Maybe (catMaybes)
import Control.Monad.State
import GHC.Conc (threadWaitRead)

-- data EndPoint = EP_Handle | EP_Pure a | EP_Void

data Process a b m r where
  SimpleProcess     :: (MonadIO m) => ByteString -> Bool -> Process a b m ()
  SourceProcess     :: (MonadIO m) => ByteString ->         Process a b m ()
  SinkProcess       :: (MonadIO m) => ByteString -> Bool -> Process a b m ()  
--  ControlledProcess :: (MonadIO m) => String -> Process EP_Handle EP_Handle m ()
  Pure              :: Pipe a b m r -> Process a b m r

data Network a b m r where
  SingleProcess :: Process a b m r -> Network a b m r
  Connect       :: Network a b m r -> Network b c m r -> Network a c m r

saveCloseFds :: (MonadState [Fd] m) => [Fd] -> m ()
saveCloseFds fds = get >>= put . (fds ++)
removeCloseFds :: (MonadState [Fd] m) => [Fd] -> m ()
removeCloseFds fds = get >>= put . (\\ fds)

singleton a = [a]

runNetwork :: (MonadIO m, MonadState [Fd] m) => Network a b m r -> Fd -> Fd -> m [CPid]
runNetwork (SingleProcess p) fdIn fdOut = do
  case p of
    SimpleProcess cmd waitRead -> executeInForkShell waitRead cmd Nothing (Just fdIn) (Just fdOut) >>= return . singleton
    SourceProcess cmd          -> executeInForkShell False    cmd Nothing Nothing (Just fdOut)     >>= return . singleton
    SinkProcess cmd waitRead   -> executeInForkShell waitRead cmd Nothing (Just fdIn) Nothing      >>= return . singleton
{-    Pure pipe -> do h1 <- liftIO $ fdToHandle fdIn
                    h2 <- liftIO $ fdToHandle fdOut
                    removeCloseFds [fdIn,fdOut]
                    runEffect $ fromHandle h1 >-> pipe >-> toHandle h2
                    return [] -}
runNetwork (Connect p1 p2) fdIn fdOut = do
  (intermRead, intermWrite) <- liftIO createPipe
  debug $ putStrLn $ "runNetwork Handle_Handle: created fds " ++ show intermRead ++ ", " ++ show intermWrite
  saveCloseFds [intermRead, intermWrite]    
  pids1 <- runNetwork p1 fdIn intermWrite
  pids2 <- runNetwork p2 intermRead fdOut
  liftIO $ mapM_ closeFd [intermRead, intermWrite]
  debug $ putStrLn $ "runNetwork Handle_Handle: close fds " ++ show intermRead ++ ", " ++ show intermWrite
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

t1 = Connect ls' (save "foo")

t2 = Connect
     (Connect ls' mkUpper)
     (save "foo")

t3 = Connect
     (Connect ls' idProc)
     (save "foo")

t4 = Connect
     (Connect ls' testWithErr)
     (save "foo")

t5 = Connect
     (Connect ls' longRunning)
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
