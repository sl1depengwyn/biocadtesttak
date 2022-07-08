module Main where

import Hydrophobicity (run)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [filename] -> run filename
    _ -> do
      putStrLn $ "Usage: " <> progName <> " <.pdb filename>"
      exitFailure
