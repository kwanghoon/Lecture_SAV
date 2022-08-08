module Expr where

--
type VarName  = String

data Const =
    CInt  Int
  | CBool Bool
  deriving Show

data Type =
    TyInt
  | TyBool
  deriving Show
  
data Op =
    OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpLessThan   -- x > y  ==> y < x 
  | OpEqual      -- x <= y ==> x < y or x == y
  | OpAnd
  | OpOr
  | OpNot
  deriving Show

-- Expressions
data Expr =
    ECst   Const
  | EVar   VarName
  | EBinOp Op Expr Expr
  | EUnaryOp Op Expr
  deriving Show

-- Commands
data Comm =
    CSkip
  | CSeq Comm Comm
  | CAssign VarName Expr
  | CRead VarName
  | CWrite VarName
  | CIf Expr Comm Comm
  | CWhile Expr Comm
  deriving Show

-- Program
data Prog = Prog Decls Comms
  deriving Show

type Decl  = (Type, VarName)
type Decls = [ Decl ]

type Comms = [ Comm ]

data AST = 
    ASTDecls { fromASTDecls :: Decls }
  | ASTDecl  { fromASTDecl  :: Decl  }
  | ASTComms { fromASTComms :: Comms }
  | ASTComm  { fromASTComm  :: Comm  }
  | ASTExpr  { fromASTExpr  :: Expr  }
  | ASTType  { fromASTType  :: Type  }
  | ASTProg  { fromASTProg  :: Prog  }
  deriving Show

commsToComm [] = CSkip
commsToComm [comm] = comm
commsToComm (comm1:comms) = CSeq comm1 (commsToComm comms)

