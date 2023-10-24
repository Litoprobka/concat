module Main (main) where

import Concat
import Relude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putTextLn "No program to run"
    program : _ -> do
      finalStack <- run $ fromString $ program
      print finalStack
