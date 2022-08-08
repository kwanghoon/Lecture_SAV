module Interp (interp) where

import Expr
import qualified Data.Map as Map
import WhileMonad
import Control.Monad (foldM)

interp :: WhileMonad m => Prog -> m ()
interp prog =
  do env <- foldM runEach env0 $ progComms prog
     prEnv env
  where tyinfo0 = Map.fromList . map swap. progDecls $ prog
        env0    = Map.empty
        
        runEach env comm = interpComm tyinfo0 env comm
        swap (x,y) = (y,x)

prEnv :: WhileMonad m => Map.Map String Const -> m ()
prEnv env = mapM_ prBind $ Map.toList env
  where prBind (var,val) =
          do writeString var
             writeString " = "
             writeString $ show val
             writeString "\n"

--
type TyInfo = Map.Map String Type
type Env = Map.Map String Const

--
interpExpr :: WhileMonad m => Env -> Expr -> m Const
interpExpr env (ECst c) = return c

interpExpr env (EVar var) =
  case Map.lookup var env of
    Nothing -> error $ "Variable not found: " ++ var
    Just c  -> return c

interpExpr env (EBinOp op expr1 expr2) =
  do val1 <- interpExpr env expr1
     val2 <- interpExpr env expr2
     interpBinOp op val1 val2
     
interpExpr env (EUnaryOp op expr) =
  do val <- interpExpr env expr
     interpUnaryOp op val


--
interpComm :: WhileMonad m => TyInfo -> Env -> Comm -> m Env
interpComm tyinfo env (CSkip) = return env

interpComm tyinfo env (CSeq comm1 comm2) =
  do env1 <- interpComm tyinfo env comm1
     env2 <- interpComm tyinfo env1 comm2
     return env2

interpComm tyinfo env (CAssign var expr) =
  do val <- interpExpr env expr
     return $ Map.insert var val env

interpComm tyinfo env (CRead var) =
  do val <- interpRead tyinfo var
     return $ Map.insert var val env

interpComm tyinfo env (CWrite var) =
  case Map.lookup var env of
    Nothing -> error $ "write: undefined variable: " ++ var
    Just val -> do interpWrite val; return env

interpComm tyinfo env (CIf expr thenComm elseComm) =
  do val <- interpExpr env expr
     case val of
       CBool True  -> interpComm tyinfo env thenComm
       CBool False -> interpComm tyinfo env elseComm
       _           -> error $ "if condition: not boolean: " ++ show val

interpComm tyinfo env (CWhile expr comm) = 
  do val <- interpExpr env expr
     case val of 
       CBool True  -> do env1 <- interpComm tyinfo env comm
                         interpComm tyinfo env1 (CWhile expr comm)
       CBool False -> return env
       _           -> error $ "if condition: not boolean: " ++ show val

--
interpRead :: WhileMonad m => TyInfo -> VarName -> m Const
interpRead tyinfo var =
  case Map.lookup var tyinfo of
    Nothing     -> error $ "read: Unknown type for: " ++ var
    Just TyInt  -> do n <- readInt; return (CInt n)
    Just TyBool -> do b <- readBool; return (CBool b)

interpWrite :: WhileMonad m => Const -> m ()
interpWrite val = writeString (show val)

--
interpBinOp :: WhileMonad m => Op -> Const -> Const -> m Const
interpBinOp OpAdd      (CInt n1)  (CInt n2)  = return . CInt  $ n1 + n2
interpBinOp OpSub      (CInt n1)  (CInt n2)  = return . CInt  $ n1 - n2
interpBinOp OpMul      (CInt n1)  (CInt n2)  = return . CInt  $ n1 * n2
interpBinOp OpDiv      (CInt n1)  (CInt n2)  = return . CInt  $ n1 `div` n2
interpBinOp OpMod      (CInt n1)  (CInt n2)  = return . CInt  $ n1 `mod` n2
interpBinOp OpLessThan (CInt n1)  (CInt n2)  = return . CBool $ n1 < n2
interpBinOp OpEqual    (CInt n1)  (CInt n2)  = return . CBool $ n1 == n2
interpBinOp OpEqual    (CBool b1) (CBool b2) = return . CBool $ b1 == b2
interpBinOp OpAnd      (CBool b1) (CBool b2) = return . CBool $ b1 && b2
interpBinOp OpOr       (CBool b1) (CBool b2) = return . CBool $ b1 || b2
interpBinOp op         c1         c2         =
  error $ "Not supported binary operator: " ++
             show op ++ " with " ++ show c1 ++ " and " ++ show c2

--  
interpUnaryOp :: WhileMonad m => Op -> Const -> m Const
interpUnaryOp OpNot (CBool b) = return . CBool $ not b
interpUnaryOp op    c         = error $ "Not supported unary operator: " ++
                                           show op ++ " with " ++ show c

