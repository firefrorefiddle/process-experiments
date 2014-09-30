{-# LANGUAGE CPP #-}
module Common where

import Control.Monad.IO.Class
import Network.Socket
import System.Posix.Types

stdInput2, stdInput3, stdInput4, stdOutput2, stdOutput3, stdOutput4 :: Fd
stdInput2 = Fd 3
stdInput3 = Fd 4
stdInput4 = Fd 5
stdOutput2 = Fd 6
stdOutput3 = Fd 7
stdOutput4 = Fd 8

socketFd (MkSocket fd _ _  _ _) = (Fd fd)

#define DEBUG

-- debug utility
debug :: (MonadIO m) => IO () -> m ()
#ifdef DEBUG
debug f = liftIO f
#else
debug _ = return ()
#endif
