{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tcp_Arrow
import ProcessUtil

import System.Posix.IO.ByteString
import System.Posix.Process
import PseudoTypes
import qualified Data.ByteString.Char8 as B

ls = SingleProcess (SourceProcess "ls")

mkUpper = SingleProcess (SimpleProcess "tr [a-z] [A-Z]" True)

idProc = SingleProcess (SimpleProcess "cat" True)
testWithErr = SingleProcess (SimpleProcess "./test" True)
longRunning = SingleProcess (SimpleProcess "./test2" True)

save fp = SingleProcess (SinkProcess ("cat > " `B.append` fp) True)

t1 = Connect ls (save "foo")

t2 = Connect
     (Connect ls mkUpper)
     (save "foo")

t3 = Connect
     (Connect ls idProc)
     (save "foo")

t4 = Connect
     (Connect ls testWithErr)
     (save "foo")

t5 = Connect
     (Connect ls longRunning)
     (save "foo")


main = do (pids, closeFds) <- runTest $ runNetwork t5 stdInput stdOutput
          mapM_ closeFd closeFds
          collectChildrenWait pids
