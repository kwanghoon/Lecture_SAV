{-# LANGUAGE TupleSections #-}

module EGraphOpt
  ( optimizeProg
  , optimizeComm
  , optimizeExpr
  , exampleExpr
  , exampleComm
  ) where

import Expr
import qualified Data.Set as S
import Data.List (minimumBy)
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Optimize every expression inside a program.
optimizeProg :: Prog -> Prog
optimizeProg (Prog decls comms) =
  Prog decls (map optimizeComm comms)

-- | Optimize one command by equality saturation-style search.
optimizeComm :: Comm -> Comm
optimizeComm comm =
  extractBestComm (saturateComm defaultCommFuel comm)

-- | Optimize one expression by equality saturation-style search.
optimizeExpr :: Expr -> Expr
optimizeExpr expr =
  extractBest (saturate defaultFuel expr)

--------------------------------------------------------------------------------
-- A very small e-graph-like saturation engine
--------------------------------------------------------------------------------

type EClass = S.Set Expr

-- | Maximum number of saturation rounds.
--
-- This is a simple bound to prevent infinite expansion.
-- Real e-graphs use hash-consing, congruence closure, rebuilding, etc.
defaultFuel :: Int
defaultFuel = 8

-- | Maximum number of command saturation rounds.
defaultCommFuel :: Int
defaultCommFuel = 8

-- | Repeatedly add equivalent expressions.
saturate :: Int -> Expr -> EClass
saturate fuel expr =
  go fuel (S.singleton expr)
  where
    go :: Int -> EClass -> EClass
    go 0 cls = cls
    go n cls =
      let cls' = closeOneStep cls
      in if cls' == cls
           then cls
           else go (n - 1) cls'

-- | One saturation step:
--   for every known expression, add expressions produced by rewrite rules,
--   and also optimize/rewrite subexpressions recursively.
closeOneStep :: EClass -> EClass
closeOneStep cls =
  S.unions
    [ cls
    , S.fromList (concatMap rewrites (S.toList cls))
    , S.fromList (concatMap congruenceExpansions (S.toList cls))
    ]

--------------------------------------------------------------------------------
-- Command saturation engine
--------------------------------------------------------------------------------

type CClass = S.Set Comm

-- | Repeatedly add equivalent commands.
saturateComm :: Int -> Comm -> CClass
saturateComm fuel comm =
  go fuel (S.singleton comm)
  where
    go :: Int -> CClass -> CClass
    go 0 cls = cls
    go n cls =
      let cls' = closeOneStepComm cls
      in if cls' == cls
           then cls
           else go (n - 1) cls'

-- | One command saturation step.
closeOneStepComm :: CClass -> CClass
closeOneStepComm cls =
  S.unions
    [ cls
    , S.fromList (concatMap commRewrites (S.toList cls))
    , S.fromList (concatMap commCongruenceExpansions (S.toList cls))
    ]

--------------------------------------------------------------------------------
-- Rewrite rules
--------------------------------------------------------------------------------

rewrites :: Expr -> [Expr]
rewrites expr =
  simplifyRules expr ++
  constantFoldingRules expr ++
  commutativityRules expr ++
  associativityRules expr

-- | Algebraic and boolean simplification rules.
simplifyRules :: Expr -> [Expr]
simplifyRules (EBinOp OpAdd e (ECst (CInt 0))) =
  [e]

simplifyRules (EBinOp OpAdd (ECst (CInt 0)) e) =
  [e]

simplifyRules (EBinOp OpSub e (ECst (CInt 0))) =
  [e]

simplifyRules (EBinOp OpMul e (ECst (CInt 1))) =
  [e]

simplifyRules (EBinOp OpMul (ECst (CInt 1)) e) =
  [e]

simplifyRules (EBinOp OpMul _ (ECst (CInt 0))) =
  [ECst (CInt 0)]

simplifyRules (EBinOp OpMul (ECst (CInt 0)) _) =
  [ECst (CInt 0)]

simplifyRules (EBinOp OpAnd e (ECst (CBool True))) =
  [e]

simplifyRules (EBinOp OpAnd (ECst (CBool True)) e) =
  [e]

simplifyRules (EBinOp OpAnd _ (ECst (CBool False))) =
  [ECst (CBool False)]

simplifyRules (EBinOp OpAnd (ECst (CBool False)) _) =
  [ECst (CBool False)]

simplifyRules (EBinOp OpOr e (ECst (CBool False))) =
  [e]

simplifyRules (EBinOp OpOr (ECst (CBool False)) e) =
  [e]

simplifyRules (EBinOp OpOr _ (ECst (CBool True))) =
  [ECst (CBool True)]

simplifyRules (EBinOp OpOr (ECst (CBool True)) _) =
  [ECst (CBool True)]

simplifyRules (EUnaryOp OpNot (EUnaryOp OpNot e)) =
  [e]

simplifyRules _ =
  []

-- | Constant folding.
constantFoldingRules :: Expr -> [Expr]
constantFoldingRules (EBinOp op (ECst c1) (ECst c2)) =
  case evalBinConst op c1 c2 of
    Just c  -> [ECst c]
    Nothing -> []

constantFoldingRules (EUnaryOp op (ECst c)) =
  case evalUnaryConst op c of
    Just c' -> [ECst c']
    Nothing -> []

constantFoldingRules _ =
  []

evalBinConst :: Op -> Const -> Const -> Maybe Const
evalBinConst OpAdd (CInt x) (CInt y) =
  Just (CInt (x + y))

evalBinConst OpSub (CInt x) (CInt y) =
  Just (CInt (x - y))

evalBinConst OpMul (CInt x) (CInt y) =
  Just (CInt (x * y))

evalBinConst OpDiv (CInt _) (CInt 0) =
  Nothing

evalBinConst OpDiv (CInt x) (CInt y) =
  Just (CInt (x `div` y))

evalBinConst OpMod (CInt _) (CInt 0) =
  Nothing

evalBinConst OpMod (CInt x) (CInt y) =
  Just (CInt (x `mod` y))

evalBinConst OpLessThan (CInt x) (CInt y) =
  Just (CBool (x < y))

evalBinConst OpEqual (CInt x) (CInt y) =
  Just (CBool (x == y))

evalBinConst OpEqual (CBool x) (CBool y) =
  Just (CBool (x == y))

evalBinConst OpAnd (CBool x) (CBool y) =
  Just (CBool (x && y))

evalBinConst OpOr (CBool x) (CBool y) =
  Just (CBool (x || y))

evalBinConst _ _ _ =
  Nothing

evalUnaryConst :: Op -> Const -> Maybe Const
evalUnaryConst OpNot (CBool b) =
  Just (CBool (not b))

evalUnaryConst _ _ =
  Nothing

-- | Commutativity rules.
--
-- These can create cycles, so saturation uses fuel.
commutativityRules :: Expr -> [Expr]
commutativityRules (EBinOp op e1 e2)
  | isCommutative op =
      [EBinOp op e2 e1]
  | otherwise =
      []
commutativityRules _ =
  []

isCommutative :: Op -> Bool
isCommutative OpAdd   = True
isCommutative OpMul   = True
isCommutative OpEqual = True
isCommutative OpAnd   = True
isCommutative OpOr    = True
isCommutative _       = False

-- | A few associativity rewrites.
--
-- These are included to show e-graph-style equality exploration.
-- The extractor's cost model will later choose a compact version.
associativityRules :: Expr -> [Expr]
associativityRules (EBinOp op (EBinOp op' a b) c)
  | op == op' && isAssociative op =
      [EBinOp op a (EBinOp op b c)]

associativityRules (EBinOp op a (EBinOp op' b c))
  | op == op' && isAssociative op =
      [EBinOp op (EBinOp op a b) c]

associativityRules _ =
  []

--------------------------------------------------------------------------------
-- Command rewrite rules
--------------------------------------------------------------------------------

commRewrites :: Comm -> [Comm]
commRewrites c =
  structuralCommRules c ++
  guardSpecializationRules c

-- | Structural rules that preserve command semantics.
structuralCommRules :: Comm -> [Comm]
structuralCommRules (CSeq CSkip c2) =
  [c2]

structuralCommRules (CSeq c1 CSkip) =
  [c1]

structuralCommRules (CSeq (CSeq c1 c2) c3) =
  [CSeq c1 (CSeq c2 c3)]

structuralCommRules (CSeq c1 (CSeq c2 c3)) =
  [CSeq (CSeq c1 c2) c3]

structuralCommRules (CIf _ c1 c2)
  | c1 == c2 =
      [c1]

structuralCommRules _ =
  []

-- | Branch/loop/assert rules driven by simplified guard expressions.
guardSpecializationRules :: Comm -> [Comm]
guardSpecializationRules (CIf cond c1 c2) =
  let cond' = optimizeExpr cond
      base = [CIf cond' c1 c2 | cond' /= cond]
      specialized = case cond' of
        ECst (CBool True)  -> [c1]
        ECst (CBool False) -> [c2]
        _                  -> []
  in base ++ specialized

guardSpecializationRules (CWhile cond body) =
  let cond' = optimizeExpr cond
      base = [CWhile cond' body | cond' /= cond]
      specialized = case cond' of
        ECst (CBool False) -> [CSkip]
        _                  -> []
  in base ++ specialized

guardSpecializationRules (CAssert cond) =
  let cond' = optimizeExpr cond
      base = [CAssert cond' | cond' /= cond]
      specialized = case cond' of
        ECst (CBool True) -> [CSkip]
        _                 -> []
  in base ++ specialized

guardSpecializationRules _ =
  []

isAssociative :: Op -> Bool
isAssociative OpAdd = True
isAssociative OpMul = True
isAssociative OpAnd = True
isAssociative OpOr  = True
isAssociative _     = False

--------------------------------------------------------------------------------
-- Congruence-like expansion
--------------------------------------------------------------------------------

-- | This is a simple substitute for real e-graph congruence closure.
--
-- For an expression such as:
--
--   (x + 0) * 1
--
-- it recursively saturates children and rebuilds parent expressions using
-- equivalent child expressions.
congruenceExpansions :: Expr -> [Expr]
congruenceExpansions (EBinOp op e1 e2) =
  let e1s = S.toList (saturate 3 e1)
      e2s = S.toList (saturate 3 e2)
  in [ EBinOp op e1' e2' | e1' <- e1s, e2' <- e2s ]

congruenceExpansions (EUnaryOp op e) =
  let es = S.toList (saturate 3 e)
  in [ EUnaryOp op e' | e' <- es ]

congruenceExpansions _ =
  []

-- | Congruence-like expansion for commands.
commCongruenceExpansions :: Comm -> [Comm]
commCongruenceExpansions CSkip =
  []

commCongruenceExpansions (CAssign v e) =
  [ CAssign v e' | e' <- S.toList (saturate 3 e) ]

commCongruenceExpansions (CRead v) =
  [CRead v]

commCongruenceExpansions (CWrite e) =
  [ CWrite e' | e' <- S.toList (saturate 3 e) ]

commCongruenceExpansions (CAssert e) =
  [ CAssert e' | e' <- S.toList (saturate 3 e) ]

commCongruenceExpansions (CSeq c1 c2) =
  let c1s = S.toList (saturateComm 3 c1)
      c2s = S.toList (saturateComm 3 c2)
  in [ CSeq c1' c2' | c1' <- c1s, c2' <- c2s ]

commCongruenceExpansions (CIf e c1 c2) =
  let es = S.toList (saturate 3 e)
      c1s = S.toList (saturateComm 3 c1)
      c2s = S.toList (saturateComm 3 c2)
  in [ CIf e' c1' c2' | e' <- es, c1' <- c1s, c2' <- c2s ]

commCongruenceExpansions (CWhile e c) =
  let es = S.toList (saturate 3 e)
      cs = S.toList (saturateComm 3 c)
  in [ CWhile e' c' | e' <- es, c' <- cs ]

--------------------------------------------------------------------------------
-- Extraction
--------------------------------------------------------------------------------

-- | Pick the lowest-cost expression from an equivalence class.
extractBest :: EClass -> Expr
extractBest cls =
  minimumBy (comparing exprCost) (S.toList cls)

-- | Pick the lowest-cost command from an equivalence class.
extractBestComm :: CClass -> Comm
extractBestComm cls =
  minimumBy (comparing commCost) (S.toList cls)

-- | Simple static cost model.
--
-- Smaller expression is considered better.
-- Multiplication/division/modulo are slightly more expensive than addition.
exprCost :: Expr -> Int
exprCost (ECst _) =
  1

exprCost (EVar _) =
  1

exprCost (EUnaryOp _ e) =
  1 + exprCost e

exprCost (EBinOp op e1 e2) =
  opCost op + exprCost e1 + exprCost e2

opCost :: Op -> Int
opCost OpAdd      = 1
opCost OpSub      = 1
opCost OpLessThan = 1
opCost OpEqual    = 1
opCost OpAnd      = 1
opCost OpOr       = 1
opCost OpNot      = 1
opCost OpMul      = 3
opCost OpDiv      = 5
opCost OpMod      = 5

-- | Command-level cost model.
--
-- Prefers fewer/simpler commands and cheaper guards/expressions.
commCost :: Comm -> Int
commCost CSkip =
  1

commCost (CAssign _ e) =
  2 + exprCost e

commCost (CRead _) =
  2

commCost (CWrite e) =
  2 + exprCost e

commCost (CAssert e) =
  2 + exprCost e

commCost (CSeq c1 c2) =
  1 + commCost c1 + commCost c2

commCost (CIf e c1 c2) =
  2 + exprCost e + commCost c1 + commCost c2

commCost (CWhile e c) =
  3 + exprCost e + commCost c

--------------------------------------------------------------------------------
-- Ord instances
--------------------------------------------------------------------------------

-- Data.Set needs Ord. Your original AST derives only Show and Generic.
-- For quick experimentation, add deriving Eq, Ord to Expr.hs.
--
-- Recommended changes in Expr.hs:
--
--   data Const = ... deriving (Show, Eq, Ord, Generic)
--   data Type  = ... deriving (Show, Eq, Ord, Generic)
--   data Op    = ... deriving (Show, Eq, Ord, Generic)
--   data Expr  = ... deriving (Show, Eq, Ord, Generic)
--   data Comm  = ... deriving (Show, Eq, Ord, Generic)
--   data Prog  = ... deriving (Show, Eq, Ord, Generic)
--
-- If you do not want to modify Expr.hs, you can define standalone deriving
-- instances, but modifying deriving clauses is simpler for class use.

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

exampleExpr :: Expr
exampleExpr =
  EBinOp OpMul
    (EBinOp OpAdd (EVar "x") (ECst (CInt 0)))
    (ECst (CInt 1))

-- optimizeExpr exampleExpr
-- ==> EVar "x"

exampleComm :: Comm
exampleComm =
  CAssign "y"
    (EBinOp OpAdd
      (EBinOp OpMul (EVar "x") (ECst (CInt 1)))
      (EBinOp OpMul (ECst (CInt 0)) (EVar "z")))

-- optimizeComm exampleComm
-- ==> CAssign "y" (EVar "x")