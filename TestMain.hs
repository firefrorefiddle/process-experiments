module Main where

import Tcp_Arrow
import System.Posix.IO.ByteString
import System.Posix.Process

main = do (pids, closeFds) <- runTest $ runHandleVoid t2 stdInput
          mapM_ closeFd closeFds
          collectChildrenWait pids