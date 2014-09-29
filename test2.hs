import System.IO
import Control.Concurrent

main = do
  threadDelay 1000000
  getContents >>= putStr