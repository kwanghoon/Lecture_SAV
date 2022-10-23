module Dataflow where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Expr

import Data.List
import Debug.Trace

--
type Label = Int

data LabeledComm =
    LCSkip
  | LCSeq Label Label
  | LCAssign VarName Expr
  | LCRead   VarName
  | LCWrite  Expr
  | LCIf     Expr Label Label
  | LCWhile  Expr Label
  deriving Show


--
data Set =
    Singleton VarName Label    -- e.g., (z,2)
  | Entry Label
  | Exit  Label
  | Diff Set VarName           -- S1 \ {(varname,l) | l in vertices}
  | Union Set Set              -- S1 U S2
  deriving (Show, Eq, Ord)

data Equation =
    Assign Set Set     -- S1 = S2
  deriving Show

--
printLabeledComm list = mapM_ putStrLn $ map pr $ list
  where
    cmp (n1,ins1) (n2,ins2)
       | n1 < n2  = LT
       | n1 == n2 = EQ
       | n1 > n2  = GT

    pr (n,ins) = show n ++ " : " ++ show ins

printEdges edges = mapM_ putStrLn $ map pr $ edges
  where
    pr (s,e) = show s ++ " -> " ++ show e

--
dataflow :: Bool -> Prog -> IO [(Int, LabeledComm)]
dataflow verbose prog =
  do if verbose
        then do putStrLn "\nLabeling statements:"
                printLabeledComm list

                putStrLn "\nControl-flow graph:"
                printEdges edges

                putStrLn "\nReverse control-flow:"
                printEdges revEdges

                putStrLn "\nStatements:"
                mapM_ (putStrLn . show) vertices

                putStrLn "\nEntries:"
                mapM_ (putStrLn . show) equEntries

                putStrLn "\nExits:"
                mapM_ (putStrLn . show) equExits

                putStrLn "\nSolution:"
                mapM_ (putStrLn . prSetVarSet) solList

        else return ()
     return list
  where
    (root, map)      = numbering 1 (mkSeq comms) Map.empty
    list             = Map.toList map
    
    (s, es, edgeSet) = cfg map root Set.empty
    edges            = Set.toList edgeSet
    
    revEdgeMap       = revCfg edges
    revEdges         = Map.toList revEdgeMap

    vertexSet        = foldl (\set elem -> Set.insert elem set)
                         Set.empty (Prelude.map fst edges ++ Prelude.map snd edges)
    vertices         = Set.toList vertexSet

    equEntries       = entries revEdges
    equExits         = exits vertices map

    solution         = solve (equEntries ++ equExits)
    solList          = Map.toList solution

    prSetVarSet (setVar, set) = show setVar ++ " = " ++ show (Set.toList set)
    
    decls = progDecls prog
    comms = progComms prog

-- Data-flow analysis

revCfg edges = foldl insert Map.empty revEdges
  where
    insert map (e,s) =
      case Map.lookup e map of
        Nothing -> Map.insert e [s] map
        Just nodes -> Map.insert e (nodes++[s]) map

    revEdges = [ (e,s) | (s,e) <- edges ]

entries revEdges = map gen revEdges
  where
    gen (n, m:ms) = Assign (Entry n) (foldl Union (Exit m) (map Exit ms))

exits vertices labeledStmts = map gen vertices
  where
    gen n =
      case Map.lookup n labeledStmts of
        Just stmt -> Assign (Exit n) (rhs n stmt)
        Nothing -> error $ "exits: not found a statement indexed by: " ++ show n

    rhs n (LCSkip) = Entry n
    rhs n (LCSeq l1 l2) = error $ "exits-rhs: not expected: " ++ "LCSeq " ++ show l1 ++ " " ++ show l2
    rhs n (LCAssign v expr) = Union (Diff (Entry n) v) (Singleton v n)
    rhs n (LCRead v) = Union (Diff (Entry n) v) (Singleton v n)
    rhs n (LCWrite v) = Entry n
    rhs n (LCIf expr l1 l2) = Entry n
    rhs n (LCWhile expr l) = Entry n

solve equations = repUntilNoChange initSol equations
  where
    setVars = [ set | Assign set _ <- equations ]
    initSol = foldl (\m sv -> Map.insert sv Set.empty m) Map.empty setVars

repUntilNoChange sol equations =
--  trace (show sol) $ 
  let (solNext, isChanged) = bigstep sol equations in
    if isChanged then repUntilNoChange solNext equations else solNext
  where
    bigstep sol equations =
      foldl (\ (sol,flag) equ-> step flag sol equ) (sol,False) equations
    
    step flag sol (Assign setVar rhs) =
      let setRhs = eval sol rhs in
      case Map.lookup setVar sol of
        Nothing -> (Map.insert setVar setRhs sol, True || flag)
        Just set' ->
          if null setRhs
          then (sol, False || flag)
          else if setRhs == set'
               then (sol, False || flag)
               else (Map.insert setVar (Set.union setRhs set') sol, True || flag)

    eval sol (Singleton v l) = Set.insert (v,l) Set.empty

    eval sol (Entry l) =
      case Map.lookup (Entry l) sol of
        Nothing -> Set.empty
        Just set -> set

    eval sol (Exit l) =
      case Map.lookup (Exit l) sol of
        Nothing -> Set.empty
        Just set -> set

    eval sol (Diff setVar var) =
      let set = eval sol setVar
          varLabelList = Set.toList set 
      in  Set.fromList [ (v,l) | (v,l) <- varLabelList, v /= var]

    eval sol (Union setVar1 setVar2) =
      let set1 = eval sol setVar1
          set2 = eval sol setVar2
      in  Set.union set1 set2


-- Control-flow graph analysis
cfg :: Map.Map Int LabeledComm -> Label -> Set.Set (Label,Label)
                -> (Label, [Label], Set.Set (Label,Label))
cfg labelMap n graph =
  case Map.lookup n labelMap of
    Nothing -> error $ "cfg: missing " ++ show n
    Just labeledComm ->
      case labeledComm of
        LCSkip -> (n, [n], graph)
        LCSeq n1 n2 ->
          (sn1, ens2, foldl insert graph2 [(en1,sn2) | en1 <- ens1])
            where (sn1, ens1, graph1) = cfg labelMap n1 graph
                  (sn2, ens2, graph2) = cfg labelMap n2 graph1
                  insert g edge = Set.insert edge g
        LCAssign v expr -> (n, [n], graph)
        LCRead v -> (n, [n], graph)
        LCWrite expr -> (n, [n], graph)
        LCIf expr nThen nElse ->
          (n, ens1++ens2, graph2)
            where graph0 = Set.insert (n,sn1) (Set.insert (n,sn2) graph)
                  (sn1, ens1, graph1) = cfg labelMap nThen graph0
                  (sn2, ens2, graph2) = cfg labelMap nElse graph1
        LCWhile expr nBody ->
          (n, [n], foldl (\set elem -> Set.insert (elem,n) set)
                      ( Set.insert (n,sn1) graph1) ens1 )
            where (sn1, ens1, graph1) = cfg labelMap nBody graph

-- Numbering statements
numberingProg map entry []           = (entry, map)
numberingProg map entry (comm:comms) =
  let (exit, map1) = numbering entry comm map in
    numberingProg map1 exit comms

mkSeq [comm] = comm
mkSeq (comm1:comm2:comms) = mkSeq (CSeq comm1 comm2 : comms)
mkSeq [] = error $ "mkSeq: Unexpected empty list"

--
numbering :: Int -> Comm -> Map.Map Int LabeledComm -> (Int, Map.Map Int LabeledComm)

numbering entry CSkip map = (entry, Map.insert entry LCSkip map)

numbering entry (CSeq comm1 comm2) map =
  let (exit1, map1) = numbering entry comm1 map
      (exit2, map2) = numbering (exit1+1) comm2 map1
  in  (exit2 + 1, Map.insert (exit2 + 1) (LCSeq exit1 exit2) map2)

numbering entry (CAssign varName expr) map =
  (entry, Map.insert entry (LCAssign varName expr) map)

numbering entry (CRead varName) map =
  (entry, Map.insert entry (LCRead varName) map)

numbering entry (CWrite expr) map =
  (entry, Map.insert entry (LCWrite expr) map)

numbering entry (CIf expr comm1 comm2) map =
  let (exit1, map1) = numbering entry comm1 map
      (exit2, map2) = numbering (exit1+1) comm2 map1
  in  (exit2+1, Map.insert (exit2+1) (LCIf expr exit1 exit2) map2)

numbering entry (CWhile expr comm) map =
  let (exit, map1) = numbering entry comm map
  in  (exit + 1, Map.insert (exit + 1) (LCWhile expr exit) map1)