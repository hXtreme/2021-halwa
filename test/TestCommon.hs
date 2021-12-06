module TestCommon where

import qualified Common as C
import qualified Datalog.Argument as Arg
import Datalog.Atom
import qualified Datalog.Constant as Const
import qualified Datalog.Literal as Lit
import qualified Datalog.Variable as Var
import Located

loc1 = Location 1 1

atomP :: [Arg.Argument] -> Atom
atomP = Atom "p"

atomQ :: [Arg.Argument] -> Atom
atomQ = Atom "q"

atomR :: [Arg.Argument] -> Atom
atomR = Atom "r"

lit :: Atom -> Lit.Literal
lit = Lit.Pos

varX = Var.Variable "X"

varY = Var.Variable "Y"

const1 = Const.Integer 1

const2 = Const.Integer 2

argX = Arg.Variable varX

argY = Arg.Variable varY

argsXY = [argX, argY]

args12 =
  [ Arg.Constant const1,
    Arg.Constant const2
  ]

argTypesBoolBool =
  [ C.Boolean,
    C.Boolean
  ]
