{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Main where

import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString (sendAll, recv)
import Network.BSD (getProtocolNumber, getHostByName, hostAddress)

import System.Console.GetOpt.Simple

import Foreign.C.Types (CInt)
import System.Posix.Process.ByteString
import System.Posix.IO.ByteString

import Control.Monad
import Control.Exception (bracketOnError)
import Control.Concurrent (forkIO)

import Data.Map ((!))
import qualified Data.ByteString.Char8 as B

import ProcessUtil
import Common

data OpMode = SendNothing
            | SendString B.ByteString
            | SendStdin
            deriving (Show)

-- borrowed and modified from Network.connectTo
connectTo hostname port = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
          he <- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
          return sock -- instead of converting it to a Handle
        )

run server port prog = do  
  sock <- connectTo server port
  shutdown sock ShutdownSend
  let fd = socketFd sock
  setFdOption fd NonBlockingRead False
  dupTo fd stdInput
--  dupTo fd stdOutput
  executeFile "/bin/sh" False ["-c",prog] Nothing

options = [ (arg,   "port",    Required,            "port to connect to.")
          , (arg,   "server",  Default "localhost", "host address or name to connect to (default=localhost).")
          , (noArg, "exec", Optional,               "execute prog.")
          ]
          
main = do
  (opts, args) <- getUsingConf options []
  let server               = dropWhile (== '=') $ opts ! "server"
  let (port :: PortNumber) = fromIntegral . read $ opts ! "port"
  let prog                 = B.pack $ opts ! "exec"
                         
  run server port prog
