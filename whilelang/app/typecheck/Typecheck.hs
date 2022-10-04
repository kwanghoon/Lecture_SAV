module Typecheck (typecheck) where

import Expr
import qualified Data.Map as Map

--
type TyEnv = Map.Map String Type

--
typecheck :: Prog -> IO ()
typecheck prog = tcProg prog

--
tcConst :: Const -> IO Type

tcConst (CInt n)  = return TyInt

tcConst (CBool b) = return TyBool


--
tcExpr :: TyEnv -> Expr -> IO Type

tcExpr tyenv (ECst cst) = tcConst cst

tcExpr tyenv (EVar var) =
  case Map.lookup var tyenv of
    Nothing -> error $ "[tcExpr] Variable " ++ var ++ " not found: " 
    Just ty -> return ty

tcExpr tyenv (EBinOp op expr1 expr2) =
  do ty1 <- tcExpr tyenv expr1
     ty2 <- tcExpr tyenv expr2

     case (op, ty1, ty2) of
       (OpAdd, TyInt, TyInt) -> return TyInt
       (OpSub, TyInt, TyInt) -> return TyInt
       (OpMul, TyInt, TyInt) -> return TyInt
       (OpDiv, TyInt, TyInt) -> return TyInt
       (OpMod, TyInt, TyInt) -> return TyInt
       
       (OpLessThan, TyInt, TyInt) -> return TyBool
       (OpEqual, TyInt, TyInt) -> return TyBool

       (OpAnd, TyBool, TyBool) -> return TyBool
       (OpOr, TyBool, TyBool) -> return TyBool

       _ -> error $ "[tcExpr] Operation " ++
                      show op ++ " not well-typed: " ++
                      show ty1 ++ ", " ++ show ty2

tcExpr tyenv (EUnaryOp OpNot expr1) =
  do ty1 <- tcExpr tyenv expr1

     case ty1 of
       TyBool -> return TyBool

--
tcComm :: TyEnv -> Comm -> IO TyEnv

tcComm tyenv CSkip = return tyenv

tcComm tyenv (CSeq comm1 comm2) =
  do tyenv1 <- tcComm tyenv comm1
     tyenv2 <- tcComm tyenv1 comm2
     return tyenv2

tcComm tyenv comm@(CAssign var expr1) =
  case Map.lookup var tyenv of
       Nothing  -> error $ "[tcComm] Variable "  ++ var ++ " not found in " ++ show comm
       Just varty ->
         do ty1 <- tcExpr tyenv expr1
            if equalType varty ty1
            then return tyenv
            else error $ "[tcComm] Variable "  ++ var ++ " has type " ++ show varty ++
                              " not compatible with: " ++ show ty1
  
tcComm tyenv comm@(CRead var) =
  case Map.lookup var tyenv of
       Nothing  -> error $ "[tcComm] Variable "  ++ var ++ " not found in " ++ show comm
       Just varty -> return tyenv

tcComm tyenv comm@(CWrite expr) =
  do ty <- tcExpr tyenv expr
     return tyenv

           
tcComm tyenv comm@(CIf condExpr comm1 comm2) =
  do ty <- tcExpr tyenv condExpr
     case ty of
       TyBool ->
         do tyenv1 <- tcComm tyenv comm1
            tyenv2 <- tcComm tyenv comm2
            return $ Map.unionWith retEqual tyenv1 tyenv2

       _ -> error $ "[tcComm] Condtion has type " ++ show ty ++ " in " ++ show comm

  where

tcComm tyenv comm@(CWhile condExpr comm1) =
  do ty <- tcExpr tyenv condExpr
     case ty of
       TyBool ->
         do tyenv1 <- tcComm tyenv comm1
            return $ Map.unionWith retEqual tyenv tyenv1
            
       _ -> error $ "[tcComm] Condition has type " ++ show ty ++ " in " ++ show comm


--
tcProg :: Prog -> IO ()

tcProg (Prog progDecls progComms) =
  do _ <- tcComms tyenv progComms
     return ()
  where
    tyenv = Map.fromList . map (\(x,y) -> (y,x)) $ progDecls

tcComms tyenv [] = return tyenv
tcComms tyenv (comm:comms) =
  do tyenv' <- tcComm tyenv comm
     tcComms tyenv' comms

--
equalType :: Type -> Type -> Bool

equalType TyInt TyInt = True

equalType TyBool TyBool = True

--
retEqual :: Type -> Type -> Type

retEqual TyInt  TyInt  = TyInt

retEqual TyBool TyBool = TyBool

retEqual ty1    ty2    = error $ "Cannot be merged: " ++ show ty1 ++ ", " ++ show ty2



