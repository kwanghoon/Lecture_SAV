module Expr where

--
type VarName  = String

data Const =
    CInt  Int
  | CBool Bool

instance Show Const where
  show (CInt n) = show n
  show (CBool True) = "true"
  show (CBool False) = "false"

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
  | CWrite Expr
  | CIf Expr Comm Comm
  | CWhile Expr Comm
  | CAssert Expr
  deriving Show

-- Program
data Prog = Prog { progDecls :: Decls, progComms :: Comms }
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

