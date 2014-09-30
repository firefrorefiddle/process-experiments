{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Char8 as B
import Foreign.C.Types (CInt)
import Control.Applicative
import Control.Monad.State
import System.IO
import System.Exit

import ProcessUtil
import Common

mkServer port = listenOn (PortNumber port)
  
waitForClient ssocket = fst <$> accept ssocket

serve port cmd = do
    ssocket <- mkServer (fromIntegral $ (read port :: Int))
    installHandler sigCHLD (Catch collectAnyChild) Nothing
    runStateT loop ssocket
  where loop :: StateT Socket IO ()
        loop = do
          ssocket <- get
          csocket <- liftIO $ waitForClient ssocket 
          runHandler (socketFd csocket) cmd
          loop

runHandler :: Fd -> [String] -> StateT Socket IO ()
runHandler cfd cmd = do
  executeInForkShell' False (B.pack $ intercalate " " cmd) Nothing (Just cfd) (Just cfd)
  liftIO $ closeFd cfd
  return ()

main = do
  args <- getArgs
  case args of
    (port:rest) -> serve port rest
