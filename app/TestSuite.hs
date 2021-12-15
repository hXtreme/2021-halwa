module TestSuite where

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
    ast <- loadFile "example/path.dl"
    putStrLn "Loaded path program and created its AST. Now comparing it to expected AST."
    let d1 = Declaration {Datalog.Declaration.predicate="path", Datalog.Declaration.argTypes=[Common.Symbol, Common.Symbol]}
        d2 = Declaration {Datalog.Declaration.predicate="edge", Datalog.Declaration.argTypes=[Common.Symbol, Common.Symbol]}        
        var1 = Datalog.Variable.Variable {name="A"}
        var2 = Datalog.Variable.Variable {name="B"}
        var7 = Datalog.Variable.Variable {name="C"}
        a1 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var2]}
        a4 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var7]}
        a5 = Atom {Datalog.Atom.predicate="edge", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var2]}
        a6 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var2, Datalog.Argument.Variable var7]}
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
        program1 = DL.Program {DL.declarations=[d1, d2], DL.rules=[rule1, rule2], DL.facts=[fact1, fact2], DL.disjunction=[], DL.queries=[query1]}        
        -- in putStrLn $ show (DL.facts program1)
        in putStrLn $ show (program1 == ast)

-- parser :: Parser DL.Program

prettyTestPath :: IO ()
prettyTestPath = 
  do
    ast <- loadFile "example/path.dl"
    putStrLn "Loaded path program and created its AST."
    let parsed = P.parse (parser :: Parser DL.Program) (pretty ast)
        prettyProg = case parsed of
                        -- Left _ -> "error"
                        Right r -> val $ r
    putStrLn $ show (DL.facts prettyProg)
    putStrLn $ show (DL.facts ast)
    -- putStrLn $ show (DL.facts prettyProg == DL.facts ast)

evenTest :: IO ()
evenTest = 
  do
    ast <- loadFile "example/even.dl"
    putStrLn "Loaded path program and created its AST. Now comparing it to expected AST."
    let d1 = Declaration {Datalog.Declaration.predicate="num", Datalog.Declaration.argTypes=[Common.Integer]}
        d2 = Declaration {Datalog.Declaration.predicate="succ", Datalog.Declaration.argTypes=[Common.Integer, Common.Integer]}
        d3 = Declaration {Datalog.Declaration.predicate="even", Datalog.Declaration.argTypes=[Common.Integer]}
        var1 = Datalog.Variable.Variable {name="A"}
        var2 = Datalog.Variable.Variable {name="B"}
        var7 = Datalog.Variable.Variable {name="C"}
        a1 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var2]}
        a4 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var7]}
        a5 = Atom {Datalog.Atom.predicate="edge", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var2]}
        a6 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var2, Datalog.Argument.Variable var7]}
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
        program1 = DL.Program {DL.declarations=[d1, d2], DL.rules=[rule1, rule2], DL.facts=[fact1, fact2], DL.disjunction=[], DL.queries=[query1]}        
        in putStrLn $ pretty program1
        -- in putStrLn $ show (program1 == ast)        

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> cli fileName
    _ -> putStrLn "Usage: stack run <datalog-file>"
