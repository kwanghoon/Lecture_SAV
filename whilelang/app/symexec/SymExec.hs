-- A symbolic execution of the While language
-- based on Tikhon Jelvis's implementation
--   https://github.com/TikhonJelvis/imp

{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module SymExec where

import           Control.Monad (foldM, forM_, (=<<), (<=<))

import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)

import           Data.String (IsString (..))

import           Z3.Monad      (AST, Z3, (+?))
import qualified Z3.Monad      as Z3

import Expr hiding (AST)

symExec :: Prog -> IO ()
symExec prog = do
  putStrLn <=< runZ3 $ do
      let decls = progDecls prog
      let comm  = commsToComm $ progComms prog
      
      let names = [ named var | (ty,var) <- decls ]
      z3vars <- mapM makeVar names
      let scope =  Map.fromList $ zip names z3vars

      commToZ3 scope comm
      Z3.solverCheckAndGetModel >>= \case
        (Z3.Sat, Just model) -> Z3.modelToString model
        (result, _)          -> return $ "Failed: " ++ show result
  where runZ3 = Z3.evalZ3With (Just Z3.QF_BV) opts


-- | The int parameter makes it easy to create new versions of a
-- variable.
newtype Name = Name String deriving (Eq, Ord)

instance Show Name where show (Name s) = s

instance IsString Name where fromString = Name

--
type Z3Var = AST

type Vars = Map Name Z3Var

named v = Name v

getZ3Var :: Name -> Vars -> Z3Var
getZ3Var name scope = case Map.lookup name scope of
  Just x  -> x
  Nothing -> error $ "Variable " ++ show name ++ " does not exist!"

makeVar :: Name -> Z3 AST
makeVar name = Z3.mkFreshBvVar (show name) width

makeVars :: [Name] -> Z3 Vars
makeVars names = foldM addVar Map.empty names
  where addVar vars name = do var <- makeVar name
                              return (Map.insert name var vars)

width :: Int
width = 32

bound :: Int
bound = 30


unroll :: Int -> Comm -> Comm
unroll bound = \case
  CWhile cond body -> unrollLoop bound (CWhile cond body)
  c_1 `CSeq` c_2   -> unroll bound c_1 `CSeq` unroll bound c_2
  CIf cond c_1 c_2 -> CIf cond (unroll bound c_1) (unroll bound c_2)
  cmd             -> cmd
  where unrollLoop 0 _                      = CSkip
        unrollLoop n loop@(CWhile cond body) =
          CIf cond (body `CSeq` unrollLoop (n - 1) loop) CSkip


constToZ3 :: Vars -> Const -> Z3 AST
constToZ3 scope = \case
  CInt n  -> Z3.mkBvNum width n
  CBool b -> Z3.mkBool b

unaryopExprToZ3 :: Vars ->  AST -> Op ->Z3 AST
unaryopExprToZ3 scope z3E1 = \case 
  OpNot -> Z3.mkNot z3E1
  op    -> error $ "unaryopExprToZ3: " ++ show op

binopExprToZ3 :: Vars ->  AST -> AST -> Op ->Z3 AST
binopExprToZ3 scope z3E1 z3E2 = \case
  OpAdd      -> Z3.mkBvadd z3E1 z3E2
  OpSub      -> Z3.mkBvsub z3E1 z3E2
  OpMul      -> Z3.mkBvmul z3E1 z3E2
  OpDiv      -> Z3.mkBvsdiv z3E1 z3E2
  OpMod      -> Z3.mkBvsmod z3E1 z3E2      -- mod vs. rem ??
  OpLessThan -> Z3.mkBvslt z3E1 z3E2
  OpEqual    -> Z3.mkEq z3E1 z3E2
  OpAnd      -> Z3.mkAnd [z3E1, z3E2]
  OpOr       -> Z3.mkOr  [z3E1, z3E2]
  -- OpNot -> Zundefined

exprToZ3 :: Vars -> Expr -> Z3 AST
exprToZ3 scope = \case
  ECst cst -> constToZ3 scope cst
  EVar var -> return . fromJust $ Map.lookup (named var) scope
  EUnaryOp op e1 ->
    do z3E1 <- exprToZ3 scope e1
       unaryopExprToZ3 scope z3E1 op
  EBinOp op e1 e2 ->
    do z3E1 <- exprToZ3 scope e1
       z3E2 <- exprToZ3 scope e2
       binopExprToZ3 scope z3E1 z3E2 op

commToZ3 :: Vars -> Comm -> Z3 Vars
commToZ3 scope = compile scope . unroll bound
  where
    compile scope = \case
      CSkip -> return scope

      CAssign var expr ->
        do let name = named var
           newVar <- makeVar name
           z3Expr <- exprToZ3 scope expr
           Z3.assert =<< Z3.mkEq newVar z3Expr
           return $ Map.insert name newVar scope

      CSeq comm1 comm2 ->
        do scope1 <- compile scope comm1
           compile scope1 comm2

      CIf cond comm1 comm2 -> 
        do z3Cond <- exprToZ3 scope cond
           scope1 <- compile scope comm1
           scope2 <- compile scope comm2
           makePhis z3Cond scope scope1 scope2

      CRead var ->
        do let name = named var
           newVar <- makeVar name
           return $ Map.insert name newVar scope

      CWrite expr ->
        do z3Expr <- exprToZ3 scope expr
           return scope
      
      CAssert cond -> 
        do z3Cond <- exprToZ3 scope cond
           z3False <- Z3.mkBool False
           Z3.assert =<< Z3.mkEq z3False z3Cond
           return scope

      comm -> error $ "Unexpected commands: " ++ show comm

-- | Encodes the result of a conditional by asserting new values for
-- each variable depending on which branch was taken. Example:
--
--  y := 10;
--  if cond { x := 1; y := y + 10 }
--     else { y := y + 11; z := y }
--
--  x_1 = 1
--  y_2 = y_1 + 10
--  y_3 = y_ 1 + 11
--  z_1 = y_3
--  x_2 = ite(cond, x_1, x_0)
--  y_4 = ite(cond, y_2, y_3)
--  z_2 = ite(cond, z_0, z_1)
makePhis :: AST -> Vars -> Vars -> Vars -> Z3 Vars
makePhis cond original scope' scope'' = foldM go original $ Map.keys original
  where go scope name = do
          newVar     <- makeVar name
          ite        <- Z3.mkIte cond (getZ3Var name scope') (getZ3Var name scope'')
          constraint <- Z3.mkEq newVar ite
          Z3.assert constraint
          return $ Map.insert name newVar scope


opts = Z3.opt "MODEL" True

constrainVars :: Map Name Int -> Vars -> Z3 ()
constrainVars values scope = forM_ (Map.keys values) $ \ name -> do
  val <- Z3.mkBvNum width (values ! name)
  Z3.assert =<< Z3.mkEq (scope ! name) val


-- forwardsExample path = z3File forwards (Map.fromList []) path

-- backwardsExample path = z3File backwards constraints gcdPath
--  where constraints = (Map.fromList [("a", 135), ("b", 135), ("d", 0)])

forwards :: Map Name Int -> Comm -> Z3 ()
forwards values program = do initialScope <- makeVars $ Map.keys values
                             constrainVars values initialScope
                             commToZ3 initialScope program
                             return ()

backwards :: Map Name Int -> Comm -> Z3 ()
backwards values program = do initialScope <- makeVars $ Map.keys values
                              finalScope   <- commToZ3 initialScope program
                              constrainVars values finalScope
                              return ()

