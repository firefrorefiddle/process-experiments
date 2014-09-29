import System.IO

main = do
  hPutStrLn stderr "hallo"
  getContents >>= putStr