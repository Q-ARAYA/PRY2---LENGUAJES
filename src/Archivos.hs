{-# LANGUAGE BlockArguments #-}
import System.IO

main = do
  fromHandle <- getAndOpenFile "Copy from: " ReadMode
  toHandle <- getAndOpenFile "Copy to: " WriteMode
  contents <- hGetContents fromHandle
  hPutStr toHandle contents
  hClose toHandle
  putStrLn "Done."

getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode = do
  putStr prompt
  hFlush stdout
  name <- getLine
  openFile name mode