module Main where

import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString (sendAll, recv)
import Network.BSD (getProtocolNumber, getHostByName, hostAddress)

import System.IO hiding (stdin, stdout)
import System.Console.GetOpt.Simple

import Control.Monad
import Control.Exception (bracketOnError)
import Control.Concurrent (forkIO)

import Pipes
import Pipes.Network.TCP (fromSocket, toSocket)
import Pipes.ByteString (fromHandle, toHandle, stdin, stdout)

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)

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

run server port opMode = do  
  sock <- connectTo server port
  -- to server
  forkIO $ do
    case opMode of
      SendNothing    -> return ()
      SendString str -> runEffect $ yield str >-> toSocket sock
      SendStdin      -> runEffect $ stdin >-> toSocket sock
    shutdown sock ShutdownSend
  -- from server
  runEffect $ fromSocket sock 4096 >-> stdout

options = [ (arg,   "port",    Required,            "port to connect to.")
          , (arg,   "server",  Default "localhost", "host address or name to connect to (default=localhost).")
          , (noArg, "command", Optional,            "Send rest of command line to server instead of stdin")
          , (noArg, "read",    Optional,            "Only read server output, don't send anything.")
          ]
          
main = do
  (opts, args) <- getUsingConf options []
  let server      = dropWhile (== '=') $ opts ! "server"
  let port        = fromIntegral . read $ opts ! "port"
  let read        = isJust $ M.lookup "read" opts
  let interactive = isJust $ M.lookup "interactive" opts
  let command     = if isJust $ M.lookup "command" opts
                    then Just $ B.intercalate (B.pack " ") (map B.pack args)
                    else Nothing
  let opMode = if (read && interactive)
               then error "--read conflicts with --interactive"
               else if read
                    then SendNothing
                    else case command of
                      Nothing -> SendStdin
                      Just cmd -> SendString cmd
                         
  run server port opMode
