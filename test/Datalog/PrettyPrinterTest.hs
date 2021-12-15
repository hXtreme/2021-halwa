-- module Datalog.PrettyPrinterTest (test_all) where
module Datalog.PrettyPrinterTest where

-- import Datalog.Program
-- import Pretty
-- import Located
-- import Parseable (Parseable (parser))
-- import Parser (Parser, parse)
-- import Test.HUnit
-- import Test.QuickCheck
-- import TestCommon
-- import Datalog.Atom (Atom)
-- import Datalog.Declaration (Declaration)
-- import Datalog.Disjunction (Disjunction)
-- import Datalog.Fact (Fact)
-- import Datalog.Query
-- import Datalog.Rule (Rule)

import qualified Datalog.Program as DL
import Parseable (parseFromFile, parser)
import Located (Located (L), locateAt, Location (Location), val)
import Parser (ParseError, Parser)
import qualified Parser as P
import Pretty (pretty)
import System.Environment (getArgs)
import Datalog.Declaration
import Datalog.Atom
import Datalog.Variable
import Datalog.Constant
import Datalog.Argument
import Datalog.Fact
import Datalog.Literal
import Datalog.Rule
import Datalog.Query
import Common
import Test.HUnit

loadFile :: FilePath -> IO DL.Program
loadFile fileName = do
  parsed <- parseFromFile fileName
  case parsed of
    Left err -> error $ pretty err
    Right prog -> return prog

prettyTestPath :: IO ()
prettyTestPath = 
  do
    ast <- loadFile "example/path.dl"
    putStrLn "Loaded path program and created its AST."
    let parsed = P.parse (parser :: Parser DL.Program) (pretty ast)
        prettyProg = case parsed of
                        -- Left _ -> "error"
                        Right r -> val $ r
    -- putStrLn $ show (DL.facts prettyProg)
    -- putStrLn $ show (DL.facts ast)
    if DL.declarations prettyProg == DL.declarations ast
        then putStrLn "GOOD declarations"
        else putStrLn ("BAD declarations\n" ++ "pretty: " ++ show (DL.declarations prettyProg) ++ "\noriginal: " ++ show (DL.declarations ast))
    if DL.facts prettyProg == DL.facts ast
        then putStrLn "GOOD facts"
        else putStrLn ("BAD Facts\n" ++ "pretty: " ++ show (DL.facts prettyProg) ++ "\noriginal: " ++ show (DL.facts ast))
    if DL.rules prettyProg == DL.rules ast
        then putStrLn "GOOD rules"
        else putStrLn ("BAD rules\n" ++ "pretty: " ++ show (DL.rules prettyProg) ++ "\noriginal: " ++ show (DL.rules ast))
    if DL.queries prettyProg == DL.queries ast
        then putStrLn "GOOD queries"
        else putStrLn ("BAD queries\n" ++ "pretty: " ++ show (DL.queries prettyProg) ++ "\noriginal: " ++ show (DL.queries ast))

prettyTestEven :: IO ()
prettyTestEven = 
  do
    ast <- loadFile "example/even.dl"
    putStrLn "Loaded even program and created its AST."
    let parsed = P.parse (parser :: Parser DL.Program) (pretty ast)
        prettyProg = case parsed of
                        -- Left _ -> "error"
                        Right r -> val $ r
    -- putStrLn $ show (DL.facts prettyProg)
    -- putStrLn $ show (DL.facts ast)
    if DL.declarations prettyProg == DL.declarations ast
        then putStrLn "GOOD declarations"
        else putStrLn ("BAD declarations\n" ++ "pretty: " ++ show (DL.declarations prettyProg) ++ "\noriginal: " ++ show (DL.declarations ast))
    if DL.facts prettyProg == DL.facts ast
        then putStrLn "GOOD facts"
        else putStrLn ("BAD Facts\n" ++ "pretty: " ++ show (DL.facts prettyProg) ++ "\noriginal: " ++ show (DL.facts ast))
    if DL.rules prettyProg == DL.rules ast
        then putStrLn "GOOD rules"
        else putStrLn ("BAD rules\n" ++ "pretty: " ++ show (DL.rules prettyProg) ++ "\noriginal: " ++ show (DL.rules ast))
    if DL.queries prettyProg == DL.queries ast
        then putStrLn "GOOD queries"
        else putStrLn ("BAD queries\n" ++ "pretty: " ++ show (DL.queries prettyProg) ++ "\noriginal: " ++ show (DL.queries ast))

prettyTestTraffic :: IO ()
prettyTestTraffic = 
  do
    ast <- loadFile "example/path.dl"
    putStrLn "Loaded path program and created its AST."
    let parsed = P.parse (parser :: Parser DL.Program) (pretty ast)
        prettyProg = case parsed of
                        -- Left _ -> "error"
                        Right r -> val $ r
    -- putStrLn $ show (DL.facts prettyProg)
    -- putStrLn $ show (DL.facts ast)
    if DL.declarations prettyProg == DL.declarations ast
        then putStrLn "GOOD declarations"
        else putStrLn ("BAD declarations\n" ++ "pretty: " ++ show (DL.declarations prettyProg) ++ "\noriginal: " ++ show (DL.declarations ast))
    if DL.facts prettyProg == DL.facts ast
        then putStrLn "GOOD facts"
        else putStrLn ("BAD Facts\n" ++ "pretty: " ++ show (DL.facts prettyProg) ++ "\noriginal: " ++ show (DL.facts ast))
    if DL.rules prettyProg == DL.rules ast
        then putStrLn "GOOD rules"
        else putStrLn ("BAD rules\n" ++ "pretty: " ++ show (DL.rules prettyProg) ++ "\noriginal: " ++ show (DL.rules ast))
    if DL.queries prettyProg == DL.queries ast
        then putStrLn "GOOD queries"
        else putStrLn ("BAD queries\n" ++ "pretty: " ++ show (DL.queries prettyProg) ++ "\noriginal: " ++ show (DL.queries ast))
