module Main where

import Network (listenOn, PortID(..), sClose)
import Network.Socket (accept, Socket(..))
import System.Environment
import Data.List
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import System.Posix.Signals
import Data.ByteString.Char8 (ByteString)
import Foreign.C.Types (CInt)
import Control.Applicative
import Control.Monad.State
import System.IO
import System.Exit

getShell = do
  env <- getEnvironment
  case lookup "SHELL" env of
    Nothing -> return "/bin/sh"
    Just sh -> return sh

socketFD (MkSocket fd _ _  _ _) = (Fd fd)

mkServer port = listenOn (PortNumber port)
  
waitForClient ssocket = fst <$> accept ssocket

collectChildren = do
    res <- getAnyProcessStatus False True
    case res of
      Nothing -> return ()
      Just (pid, pStatus) ->
        case pStatus of
          Exited ecode ->
            if ecode /= ExitSuccess
            then hPutStrLn stderr $ show pid ++ " exited with " ++ show pStatus
            else return ()
          Terminated _ True  -> hPutStrLn stderr $ show pid ++ " crashed with core dump"
          Terminated s False -> hPutStrLn stderr $ show pid ++ " terminated through signal " ++ show s
          Stopped s          -> hPutStrLn stderr $ show pid ++ " stopped through signal " ++ show s

serve port cmd = do
    ssocket <- mkServer (fromIntegral $ (read port :: Int))
    installHandler sigCHLD (Catch collectChildren) Nothing
    runStateT loop ssocket
  where loop :: StateT Socket IO ()
        loop = do
          ssocket <- get
          csocket <- liftIO $ waitForClient ssocket 
          runHandler (socketFD csocket) cmd
          loop

executeInFork file searchPath args env cfd = do
  ssocket <- get
  liftIO $ forkProcess $ do
    sClose ssocket
    closeFd stdInput
    closeFd stdOutput  
    dupTo cfd stdInput
    dupTo cfd stdOutput
    setFdOption cfd NonBlockingRead False
    executeFile file searchPath args env

runHandler :: Fd -> [String] -> StateT Socket IO ()
runHandler cfd cmd = do
  shell <- liftIO getShell
  executeInFork shell False ["-c", intercalate " " cmd] Nothing cfd
  liftIO $ closeFd cfd
  return ()

main = do
  args <- getArgs
  case args of
    (port:rest) -> serve port rest
