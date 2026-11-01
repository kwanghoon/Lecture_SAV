-- | Sign analysis for the WHILE language (Expr.hs).
--
-- Build with the existing project's Expr module on the source path.
-- The analysis is flow-sensitive and uses least fixed points at loops.
-- An unreachable control-flow path is represented by Nothing (not by a
-- variable having sign NONE). A missing variable in a reachable state
-- has sign ANY (unknown), not NONE (no possible integer values).
module Sign
  ( Sign(..), Truth(..), PState, AState
  , bottom, top, joinSign, meetSign, leqSign
  , lookupSign, updateSign, joinState, meetState
  , evalA, evalB, assumeTrue, assumeFalse
  , analyze, analyzeProg, analyzeComm, loopTrace, example
  , analyzeSign, showAState
  ) where

import qualified Data.Map.Strict as Map
import Data.Bits ((.&.), (.|.))
import Data.List (foldl')
import Data.Map (toList)
import Expr

import Debug.Trace

-- 1. Sign domain -------------------------------------------------------------

data Sign = NONE | NEG | ZERO | POS | NON_POS | NON_ZERO | NON_NEG | ANY
  deriving (Eq, Ord, Show, Enum, Bounded)

-- The three bits represent {negative, zero, positive}.
mask :: Sign -> Int
mask NONE     = 0
mask NEG      = 1
mask ZERO     = 2
mask POS      = 4
mask NON_POS  = 3
mask NON_ZERO = 5
mask NON_NEG  = 6
mask ANY      = 7

fromMask :: Int -> Sign
fromMask 0 = NONE
fromMask 1 = NEG
fromMask 2 = ZERO
fromMask 3 = NON_POS
fromMask 4 = POS
fromMask 5 = NON_ZERO
fromMask 6 = NON_NEG
fromMask _ = ANY

bottom, top :: Sign
bottom = NONE
top    = ANY

joinSign, meetSign :: Sign -> Sign -> Sign
joinSign a b = fromMask (mask a .|. mask b)
meetSign a b = fromMask (mask a .&. mask b)

leqSign :: Sign -> Sign -> Bool
leqSign a b = meetSign a b == a

atoms :: Sign -> [Sign]
atoms s = [a | a <- [NEG, ZERO, POS], mask s .&. mask a /= 0]

joinSigns :: [Sign] -> Sign
joinSigns = foldl' joinSign NONE

signOf :: Int -> Sign
signOf n | n < 0     = NEG
         | n == 0    = ZERO
         | otherwise = POS

-- A sound table for addition and multiplication of atomic signs.
addAtom :: Sign -> Sign -> Sign
addAtom ZERO b = b
addAtom a ZERO = a
addAtom NEG NEG = NEG
addAtom POS POS = POS
addAtom _ _     = ANY

negAtom :: Sign -> Sign
negAtom NEG  = POS
negAtom POS  = NEG
negAtom ZERO = ZERO
negAtom _    = ANY

negSign :: Sign -> Sign
negSign = joinSigns . map negAtom . atoms

mulAtom :: Sign -> Sign -> Sign
mulAtom ZERO _    = ZERO
mulAtom _ ZERO    = ZERO
mulAtom NEG NEG   = POS
mulAtom POS POS   = POS
mulAtom NEG POS   = NEG
mulAtom POS NEG   = NEG
mulAtom _ _       = ANY

liftSign2 :: (Sign -> Sign -> Sign) -> Sign -> Sign -> Sign
liftSign2 f a b = joinSigns [f x y | x <- atoms a, y <- atoms b]

addSign, subSign, mulSign :: Sign -> Sign -> Sign
addSign = liftSign2 addAtom
subSign a b = addSign a (negSign b)
mulSign = liftSign2 mulAtom

-- Precise transfer for integer +/- 1 (not POS - POS = ANY).
minusOne :: Sign -> Sign
minusOne = joinSigns . map step . atoms
  where
    step NEG  = NEG
    step ZERO = NEG
    step POS  = NON_NEG  -- x >= 1 => x-1 >= 0
    step _    = ANY

plusOne :: Sign -> Sign
plusOne = joinSigns . map step . atoms
  where
    step NEG  = NON_POS -- x <= -1 => x+1 <= 0
    step ZERO = POS
    step POS  = POS
    step _    = ANY

-- 2. Abstract states ---------------------------------------------------------
-- Sparse map: absent variables have sign ANY. Use Map.empty initially;
-- NONE is reserved for an impossible value, not for uninitialized variables.
-- Nothing denotes an unreachable program point (state bottom).

type PState = Map.Map VarName Sign
type AState = Maybe PState

lookupSign :: VarName -> PState -> Sign
lookupSign x ps = Map.findWithDefault ANY x ps

updateSign :: VarName -> Sign -> PState -> PState
updateSign x ANY  = Map.delete x
updateSign x sign = Map.insert x sign

joinPState :: PState -> PState -> PState
joinPState a b =
  foldl' update Map.empty (Map.keys (Map.union a b))
  where
    update result x = updateSign x (joinSign (lookupSign x a) (lookupSign x b)) result

joinState :: AState -> AState -> AState
joinState Nothing  b        = b
joinState a        Nothing  = a
joinState (Just a) (Just b) = Just (joinPState a b)

meetState :: AState -> AState -> AState
meetState Nothing _ = Nothing
meetState _ Nothing = Nothing
meetState (Just a) (Just b) =
  let vars = Map.keys (Map.union a b)
      pairs = [(x, meetSign (lookupSign x a) (lookupSign x b)) | x <- vars]
  in if any ((== NONE) . snd) pairs
       then Nothing
       else Just (foldl' (\m (x, s) -> updateSign x s m) Map.empty pairs)

refineVar :: VarName -> Sign -> AState -> AState
refineVar _ _ Nothing = Nothing
refineVar x desired (Just ps) =
  case meetSign (lookupSign x ps) desired of
    NONE -> Nothing
    s    -> Just (updateSign x s ps)

showAState :: AState -> String
showAState Nothing = "[(_,NONE)]"
showAState (Just m) = show (toList m ++ [("_",ANY)])

-- 3. Expressions -------------------------------------------------------------

evalA :: Expr -> PState -> Sign
evalA (ECst (CInt n)) _ = signOf n
evalA (EVar x) ps = lookupSign x ps
-- A precise constant-transfer rule, used in the motivating example.
evalA (EBinOp OpSub e (ECst (CInt 1))) ps = minusOne (evalA e ps)
evalA (EBinOp OpAdd e (ECst (CInt 1))) ps = plusOne (evalA e ps)
evalA (EBinOp OpAdd (ECst (CInt 1)) e) ps = plusOne (evalA e ps)
evalA (EBinOp op a b) ps =
  let x = evalA a ps
      y = evalA b ps
  in case op of
       OpAdd -> addSign x y
       OpSub -> subSign x y
       OpMul -> mulSign x y
       -- Without a relational numeric domain, use a safe over-approximation.
       OpDiv -> if x == NONE || y == NONE then NONE else ANY
       OpMod -> if x == NONE || y == NONE then NONE else ANY
       _     -> ANY
evalA _ _ = ANY  -- boolean expression in an arithmetic position

-- 4. Branch filtering --------------------------------------------------------
-- A condition is converted into a refinement of the incoming state.
-- Integer intervals for the three atomic signs are used ONLY to determine
-- whether an atom can satisfy a comparison against an integer constant.
-- This is NOT a full interval abstract interpreter.

type Bounds = (Maybe Integer, Maybe Integer)  -- Nothing = unbounded

bounds :: Sign -> Bounds
bounds NEG  = (Nothing, Just (-1))
bounds ZERO = (Just 0, Just 0)
bounds POS  = (Just 1, Nothing)
bounds _    = (Nothing, Nothing)

possibleLt :: Bounds -> Integer -> Bool
possibleLt (lo, _) n = maybe True (< n) lo

possibleGe :: Bounds -> Integer -> Bool
possibleGe (_, hi) n = maybe True (>= n) hi

possibleGt :: Bounds -> Integer -> Bool
possibleGt (_, hi) n = maybe True (> n) hi

possibleLe :: Bounds -> Integer -> Bool
possibleLe (lo, _) n = maybe True (<= n) lo

possibleEq :: Bounds -> Integer -> Bool
possibleEq (lo, hi) n =
  maybe True (<= n) lo && maybe True (>= n) hi

possibleNe :: Bounds -> Integer -> Bool
possibleNe (Just lo, Just hi) n = lo /= n || hi /= n
possibleNe _ _ = True

-- Is an atomic sign consistent with the requested comparison outcome?
possible :: Op -> Bool -> Bool -> Integer -> Sign -> Bool
possible op isVarLeft truth n a =
  let b = bounds a
  in case (op, isVarLeft, truth) of
       (OpLessThan, True,  True)  -> possibleLt b n  -- x < n
       (OpLessThan, True,  False) -> possibleGe b n  -- x >= n
       (OpLessThan, False, True)  -> possibleGt b n  -- n < x
       (OpLessThan, False, False) -> possibleLe b n  -- n >= x
       (OpEqual, _, True)        -> possibleEq b n
       (OpEqual, _, False)       -> possibleNe b n
       _                         -> True

filterComparison :: Op -> Bool -> Bool -> VarName -> Int -> AState -> AState
filterComparison op varLeft truth x n st =
  let allowed = joinSigns [a | a <- [NEG, ZERO, POS],
                              possible op varLeft truth (toInteger n) a]
  in refineVar x allowed st

-- When an AST does not match a refinement rule, return the original state.
-- This is a sound conservative fallback; it does NOT claim the condition
-- necessarily holds. And/Or are handled using their logical branches.
assume :: Bool -> Expr -> AState -> AState
assume _ _ Nothing = Nothing
assume truth (ECst (CBool b)) st
  | truth == b = st
  | otherwise  = Nothing
assume truth (EUnaryOp OpNot e) st = assume (not truth) e st
assume truth (EBinOp OpAnd a b) st
  | truth = assume True b (assume True a st)
  | otherwise = joinState (assume False a st)
                          (assume False b (assume True a st))
assume truth (EBinOp OpOr a b) st
  | truth = joinState (assume True a st)
                      (assume True b (assume False a st))
  | otherwise = assume False b (assume False a st)
assume truth (EBinOp op (EVar x) (ECst (CInt n))) st
  | op == OpLessThan || op == OpEqual =
      filterComparison op True truth x n st
assume truth (EBinOp op (ECst (CInt n)) (EVar x)) st
  | op == OpLessThan || op == OpEqual =
      filterComparison op False truth x n st
assume truth (EBinOp op (ECst (CInt n)) (ECst (CInt m))) st =
  case op of
    OpLessThan -> if truth == (n < m) then st else Nothing
    OpEqual    -> if truth == (n == m) then st else Nothing
    _          -> st
assume _ _ st = st

assumeTrue, assumeFalse :: Expr -> AState -> AState
assumeTrue  = assume True
assumeFalse = assume False

-- A Boolean value is represented by a separate type to avoid confusing
-- sign POS/NEG with truth values.
data Truth = BOTTOM | TT | FF | UNKNOWN deriving (Eq, Show)

evalB :: Expr -> PState -> Truth
evalB e ps =
  case (assumeTrue e (Just ps), assumeFalse e (Just ps)) of
    (Nothing, Nothing) -> BOTTOM
    (Just _, Nothing)  -> TT
    (Nothing, Just _)  -> FF
    (Just _, Just _)   -> UNKNOWN

-- 5. Commands and the least fixed point -------------------------------------

analyze :: Comm -> AState -> AState
analyze _ Nothing = Nothing
analyze CSkip st = st
analyze (CSeq c1 c2) st = analyze c2 (analyze c1 st)
analyze (CAssign x e) (Just ps) =
  let s = evalA e ps
  in if s == NONE then Nothing else Just (updateSign x s ps)
analyze (CRead x) (Just ps) = Just (updateSign x ANY ps)
analyze (CWrite _) st = st
analyze (CAssert cond) st = assumeTrue cond st
analyze (CIf cond yes no) st =
  joinState (analyze yes (assumeTrue cond st))
            (analyze no  (assumeFalse cond st))
analyze (CWhile cond body) st =
  let headState = loopFix cond body st
      st' = assumeFalse cond headState
  in trace ("while: " ++ showAState st') st'

-- H(head) = entry JOIN analyze(body, assumeTrue(cond, head)).
-- Since the state lattice has finite height, iteration from entry stabilizes.
loopStep :: Expr -> Comm -> AState -> AState -> AState
loopStep cond body entry headState =
  joinState entry (analyze body (assumeTrue cond headState))

loopFix :: Expr -> Comm -> AState -> AState
loopFix cond body entry = go entry
  where
    go headState =
      let next = loopStep cond body entry headState
      in if next == headState then headState else go next

-- | Successive loop-head states, including the stable fixed point.
loopTrace :: Expr -> Comm -> PState -> [PState]
loopTrace cond body entry = go (Just entry)
  where
    go current =
      case current of
        Nothing -> []
        Just ps ->
          let next = loopStep cond body (Just entry) current
          in ps : if next == current then [] else go next

analyzeComm :: Comm -> PState -> AState
analyzeComm c = analyze c . Just

analyzeProg :: Prog -> AState
analyzeProg prog = analyze (commsToComm (progComms prog)) (Just Map.empty)

analyzeSign prog = analyzeProg prog

-- | Motivating program: x := 3; while 0 < x do x := x - 1
--
-- >>> let (p, cond, body) = example
-- >>> loopTrace cond body (Map.singleton "x" POS)
-- [fromList [("x",POS)],fromList [("x",NON_NEG)]]
-- >>> analyzeProg p
-- Just (fromList [("x",ZERO)])
example :: (Prog, Expr, Comm)
example = (Prog [(TyInt, "x")] [CAssign "x" (int 3), CWhile cond body], cond, body)
  where
    int n = ECst (CInt n)
    cond = EBinOp OpLessThan (int 0) (EVar "x")
    body = CAssign "x" (EBinOp OpSub (EVar "x") (int 1))
