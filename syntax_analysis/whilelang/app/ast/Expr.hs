module Expr where

--
type VarName  = String

data Const =
    CInt  Int
  | CBool Bool
  deriving Show
  
data BinOp =
    BAdd
  | BSub
  | BMul
  | BDiv
  | BLessThan   -- x > y  ==> y < x 
  | BEqual      -- x <= y ==> x < y or x = y
  | BAnd
  | BOr
  | BNot
  deriving Show

-- Expressions
data Expr =
    ECst   Const
  | EVar   VarName
  | EBinOp BinOp Expr Expr
  deriving Show

-- Commands
data Comm =
    CSkip
  | CSeq Comm Comm
  | CAssign VarName Expr
  | CInput VarName
  | CWhile Expr Comm
  deriving Show

