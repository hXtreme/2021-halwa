module Main where

import qualified Datalog.Program as DL
import Parseable (parseFromFile)
import Pretty (pretty)
import System.Environment (getArgs)
import qualified Even as Demo.Even
import qualified FrogEngine.Lib as Engine

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
    ["demo", "evens", num] -> runEvensDemo num
    _ -> putStrLn "Usage: stack run <datalog-file>"

runEvensDemo :: String -> IO ()
runEvensDemo num = do
  putStrLn $ Engine.prettyResults execResult
  where
    base = read num :: Int
    program = Demo.Even.evens base
    execResult = Engine.exec program
