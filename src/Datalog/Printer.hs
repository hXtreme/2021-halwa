module Datalog.Printer where

import Datalog.Program
import Datalog.Declaration
import Datalog.Rule
import Datalog.Fact
import Datalog.Disjunction
import Datalog.Query
import Common (Type)

import Data.List


-- data Program = Program
--   { declarations :: [Declaration],
--     rules :: [Rule],
--     facts :: [Fact],
--     disjunction :: [Disjunction],
--     queries :: [Query]
--   }
--   deriving (Eq, Show)

main :: IO ()
main = putStrLn "Hello, World!"

printProgram :: Program -> IO ()
printProgram pr = do
                    printDeclarations (declarations pr)
                    -- printRules (rules pr)
                    -- printFacts (facts pr)
                    -- printDisjunctions (disjunctions pr)
                    -- printQueries (queries pr)
                    -- return ()

printDeclarations :: [Declaration] -> IO ()
printDeclarations ds = sequence_ (map (\x -> printDecl x) ds)

printDecl :: Declaration -> IO ()
printDecl d = putStrLn (".decl " ++ (predicate d) ++ (printArgs (typeToString (argTypes d))))

typeToString :: [Type] -> [String]
typeToString ts = map (\x -> show x) ts

printArgs :: [String] -> String
printArgs s = "(" ++ (intercalate ", " s) ++ ")"