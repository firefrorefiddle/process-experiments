{-# LANGUAGE
    GADTs,
    ExistentialQuantification,
    OverloadedStrings,
    NoMonomorphismRestriction,
    FlexibleContexts
    #-}

module Tcp_Arrow where

import Network
import System.IO
import Data.ByteString (ByteString)
import System.Posix.Types
import System.Posix.ByteString.FilePath
import System.Posix.Signals
import System.Posix.IO.ByteString
import qualified Data.ByteString as B
import Pipes
import Pipes.ByteString (fromHandle, toHandle)
import Control.Monad.State
import Data.List ((\\))

import ProcessUtil
import Common

-- data EndPoint = EP_Handle | EP_Pure a | EP_Void

data Process a b m where
  SimpleProcess     :: (MonadIO m) => ByteString -> Bool -> Process a b m
  SourceProcess     :: (MonadIO m) => ByteString ->         Process a b m
  SinkProcess       :: (MonadIO m) => ByteString -> Bool -> Process a b m
--  ControlledProcess :: (MonadIO m) => String -> Process EP_Handle EP_Handle m ()
  Pure              :: Pipe ByteString ByteString m r -> Process a b m

data Network m a b where
  SingleProcess :: Process a b m -> Network m a b
  Connect       :: Network m a b -> Network m b c -> Network m a c

saveCloseFds :: (MonadState [Fd] m) => [Fd] -> m ()
saveCloseFds fds = get >>= put . (fds ++)
removeCloseFds :: (MonadState [Fd] m) => [Fd] -> m ()
removeCloseFds fds = get >>= put . (\\ fds)

singleton a = [a]

runNetwork :: (MonadIO m, MonadState [Fd] m) => Network m a b -> Fd -> Fd -> m [CPid]
runNetwork (SingleProcess p) fdIn fdOut = do
  case p of
    SimpleProcess cmd waitRead -> executeInForkShell waitRead cmd Nothing (Just fdIn) (Just fdOut) >>= return . singleton
    SourceProcess cmd          -> executeInForkShell False    cmd Nothing Nothing (Just fdOut)     >>= return . singleton
    SinkProcess cmd waitRead   -> executeInForkShell waitRead cmd Nothing (Just fdIn) Nothing      >>= return . singleton
    Pure pipe -> do h1 <- liftIO $ fdToHandle fdIn
                    h2 <- liftIO $ fdToHandle fdOut
                    removeCloseFds [fdIn,fdOut]
                    runEffect $ fromHandle h1 >-> (pipe >> return ()) >-> toHandle h2
                    return []
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
