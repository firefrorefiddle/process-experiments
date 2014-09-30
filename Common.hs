{-# LANGUAGE CPP #-}
module Common where

import Control.Monad.IO.Class
import Network.Socket
import System.Posix.Types

socketFd (MkSocket fd _ _  _ _) = (Fd fd)

#define DEBUG

-- debug utility
debug :: (MonadIO m) => IO () -> m ()
#ifdef DEBUG
debug f = liftIO f
#else
debug _ = return ()
#endif
