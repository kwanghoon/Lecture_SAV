
{-# LANGUAGE DeriveGeneric #-}
module Expr where

-- | For printing WHILE programs in JSON format
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.Encode.Pretty (encodePretty)

import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy.Char8 as B
----------------------------------------------------------

-- | Functions for printing WHILE programs in JSON format
toJson :: Prog -> B.ByteString
toJson = encodePretty
----------------------------------------------------------

--
type VarName  = String

-- | Constants
data Const =
    CInt  Int
  | CBool Bool
  deriving (Show, Generic)

-- instance Show Const where
--   show (CInt n) = show n
--   show (CBool True) = "true"
--   show (CBool False) = "false"

instance ToJSON Const where
  toJSON (CInt n) = object [fromString "CInt" .= n]
  toJSON (CBool b) = object [fromString "CBool" .= b ]

-- | Types
data Type =
    TyInt
  | TyBool
  deriving (Show, Generic)

-- instance Show Type where
--   show TyInt = "int"
--   show TyBool = "bool"

instance ToJSON Type where
  toJSON TyInt = object [fromString "TyInt" .= ()]
  toJSON TyBool = object [fromString "TyBool" .= ()]
  
-- | Operators
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
  deriving (Show, Generic)

-- instance Show Op where
--   show OpAdd = "+"
--   show OpSub = "-"
--   show OpMul = "*"
--   show OpDiv = "/"
--   show OpMod = "%"
--   show OpLessThan = "<"
--   show OpEqual = "=="
--   show OpAnd = "&&"
--   show OpOr = "||"
--   show OpNot = "!"

instance ToJSON Op where
  toJSON OpAdd = object [fromString "OpAdd" .= ()]
  toJSON OpSub = object [fromString "OpSub" .= ()]
  toJSON OpMul = object [fromString "OpMul" .= ()]
  toJSON OpDiv = object [fromString "OpDiv" .= ()]
  toJSON OpMod = object [fromString "OpMod" .= ()]
  toJSON OpLessThan = object [fromString "OpLessThan" .= ()]
  toJSON OpEqual = object [fromString "OpEqual" .= ()]
  toJSON OpAnd = object [fromString "OpAnd" .= ()]
  toJSON OpOr = object [fromString "OpOr" .= ()]
  toJSON OpNot = object [fromString "OpNot" .= ()]

-- Expressions
data Expr =
    ECst   Const
  | EVar   VarName
  | EBinOp Op Expr Expr
  | EUnaryOp Op Expr
  deriving (Show, Generic)

instance ToJSON Expr where
  toJSON (ECst c) = object [fromString "ECst" .= c]
  toJSON (EVar v) = object [fromString "EVar" .= v]
  toJSON (EBinOp op e1 e2) = object [fromString "EBinOp" .= (op, e1, e2)]
  toJSON (EUnaryOp op e) = object [fromString "EUnaryOp" .= (op, e)]

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
  deriving (Show, Generic)

instance ToJSON Comm where
  toJSON CSkip = object [fromString "CSkip" .= ()]
  toJSON (CSeq c1 c2) = object [fromString "CSeq" .= (c1, c2)]
  toJSON (CAssign v e) = object [fromString "CAssign" .= (v, e)]
  toJSON (CRead v) = object [fromString "CRead" .= v]
  toJSON (CWrite e) = object [fromString "CWrite" .= e]
  toJSON (CIf e c1 c2) = object [fromString "CIf" .= (e, c1, c2)]
  toJSON (CWhile e c) = object [fromString "CWhile" .= (e, c)]
  toJSON (CAssert e) = object [fromString "CAssert" .= e]  

-- Program
data Prog = Prog { progDecls :: Decls, progComms :: Comms }
  deriving (Show, Generic)

instance ToJSON Prog where
  toJSON (Prog decls comms) = object [fromString "Prog" .= (decls, comms)]  

type Decl  = (Type, VarName)
type Decls = [ Decl ]

type Comms = [ Comm ]

prJsonFromExpr expr = B.putStrLn $ toJson expr

-- | AST
data AST = 
    ASTDecls { fromASTDecls :: Decls }
  | ASTDecl  { fromASTDecl  :: Decl  }
  | ASTComms { fromASTComms :: Comms }
  | ASTComm  { fromASTComm  :: Comm  }
  | ASTExpr  { fromASTExpr  :: Expr  }
  | ASTType  { fromASTType  :: Type  }
  | ASTProg  { fromASTProg  :: Prog  }
  deriving (Show, Generic)

commsToComm [] = CSkip
commsToComm [comm] = comm
commsToComm (comm1:comms) = CSeq comm1 (commsToComm comms)

