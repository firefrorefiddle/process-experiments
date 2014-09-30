{-# LANGUAGE CPP #-}
module Common where

import Control.Monad.IO.Class

-- debug utility
debug :: (MonadIO m) => IO () -> m ()
#ifdef DEBUG
debug f = liftIO f
#else
debug _ = return ()
#endif
