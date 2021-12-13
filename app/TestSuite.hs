module TestSuite where

import qualified Datalog.Program as DL
import Parseable (parseFromFile)
import Pretty (pretty)
import System.Environment (getArgs)
import Datalog.Declaration
import Datalog.Atom
import Datalog.Constant
import Datalog.Argument
import Datalog.Fact
import Datalog.Literal
import Datalog.Rule
import Datalog.Query
import Common

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

pathTest :: IO ()
pathTest = 
  do
    ast <- loadFile "/Users/andrewoch/Desktop/Haskell-Workspace/2021-halwa/example/path.dl"
    putStrLn "Loaded path program and created its AST. Now comparing it to expected AST."
    let d1 = Declaration {Datalog.Declaration.predicate="path", Datalog.Declaration.argTypes=[Common.Symbol, Common.Symbol]}
        d2 = Declaration {Datalog.Declaration.predicate="edge", Datalog.Declaration.argTypes=[Common.Symbol, Common.Symbol]}        
        const1 = Datalog.Constant.Symbol "A"
        const2 = Datalog.Constant.Symbol "B"
        const7 = Datalog.Constant.Symbol "C"
        a1 = Atom {Datalog.Atom.predicate="path", args=[Constant const1, Constant const2]}
        a4 = Atom {Datalog.Atom.predicate="path", args=[Constant const1, Constant const7]}
        a5 = Atom {Datalog.Atom.predicate="edge", args=[Constant const1, Constant const2]}
        a6 = Atom {Datalog.Atom.predicate="path", args=[Constant const2, Constant const7]}
        lit1 = Datalog.Literal.Pos a5
        lit2 = Datalog.Literal.Pos a6
        rule1 = Rule {Datalog.Rule.head=a1, body=[lit1]}
        rule2 = Rule {Datalog.Rule.head=a4, body=[lit2, lit1]}
        const3 = Datalog.Constant.Symbol "Towne"
        const4 = Datalog.Constant.Symbol "Levine"
        const5 = Datalog.Constant.Symbol "Levine"
        const6 = Datalog.Constant.Symbol "Moore"        
        a2 = Atom {Datalog.Atom.predicate="edge", args=[Constant const3, Constant const4]}
        a3 = Atom {Datalog.Atom.predicate="edge", args=[Constant const5, Constant const6]}
        fact1 = Fact {Datalog.Fact.head=a2}
        fact2 = Fact {Datalog.Fact.head=a3}
        a7 = Atom {Datalog.Atom.predicate="path", args=[]}
        query1 = Query {Datalog.Query.query=a7}
        -- lit1 = Pos a1
        -- rule1 = DL.Rule {Datalog.Rule.head=a1, body=[lit1, lit1]}
        -- query1 = DL.Query {DL.Query.query=a1}
        -- declarations = [d1, d2]
        -- rules = [rule1, rule1]
        -- facts = [fact1, fact1, fact1]
        -- queries = [query1, query1]
        -- disjunctions = []
        -- program1 = DL.Program {DL.declarations=declarations, DL.rules=rules, DL.facts=facts, DL.queries=queries, DL.disjunction=disjunctions}
        program1 = DL.Program {DL.declarations=[d1, d2], DL.rules=[rule1, rule2], DL.facts=[fact1, fact2], DL.disjunction=[], DL.queries=[query1]}        
        in putStrLn $ pretty program1     
    -- putStrLn $ pretty ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> cli fileName
    _ -> putStrLn "Usage: stack run <datalog-file>"
