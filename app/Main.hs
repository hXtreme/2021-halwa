module Main where

import qualified Datalog.Program as DL
import Parseable (parseFromFile)
import Pretty (pretty)
import System.Environment (getArgs)

loadFile :: FilePath -> IO DL.Program
loadFile fileName = do
  parsed <- parseFromFile fileName
  case parsed of
    Left err -> error $ pretty err
    Right prog -> return prog

cli :: String -> IO ()
cli fileName =
  do
    ast <- loadFile fileName
    putStrLn "Loaded program:"
    putStrLn $ pretty ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> cli fileName
    _ -> putStrLn "Usage: stack run <datalog-file>"
