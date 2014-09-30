module Main where

import Tcp_Arrow
import System.Posix.IO.ByteString
import System.Posix.Process

main = do (pids, closeFds) <- runTest $ runNetwork t5 stdInput stdOutput
          mapM_ closeFd closeFds
          collectChildrenWait pids
