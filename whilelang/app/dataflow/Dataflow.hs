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
  | LCAssert Expr
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
                putStrLn (show cfgRoot)
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
    decls = progDecls prog
    comms = progComms prog

    (root, labelMap) = numbering 1 (mkSeq comms) Map.empty
    list             = Map.toList labelMap
    
    (cfgRoot, edgeSet) = computeCfg  labelMap root Set.empty
    edges            = Set.toList edgeSet
    
    revEdges         = Map.toList (revCfg edges)

    vertexSet        = foldl (\set elem -> Set.insert elem set)
                         Set.empty (Prelude.map fst edges ++ Prelude.map snd edges)
    vertices         = Set.toList vertexSet

    equEntries       = entries revEdges
    equExits         = exits vertices labelMap

    solution         = solve (equEntries ++ equExits)
    solList          = Map.toList solution

    prSetVarSet (setVar, set) = show setVar ++ " = " ++ show (Set.toList set)

-- Data-flow analysis

-- | reverse control flow graph
-- 주어진 노드 -> 그 노드로 들어오는 에지를 갖고 있는 노드들
--   예) 4 -> 17
--       7 -> 17
--       14 -> 17
--
--       17 -> [4,7,14]

revCfg edges = foldl insert Map.empty revEdges
  where
    insert map (e,s) =
      case Map.lookup e map of
        Nothing -> Map.insert e [s] map
        Just nodes -> Map.insert e (nodes++[s]) map

    revEdges = [ (e,s) | (s,e) <- edges ]

-- | entries
-- 입력
--   revEdges : 역 제어 흐름 그래프 에지 리스트
--
-- 출력
--   Entry(n)에 대한 등식
--
-- 알고리즘
--   주어진 노드로 들어오는 노드들(Exit)의 모든 정보를(합집합)을 그 노드(Entry)로 할당

entries revEdges = map gen revEdges
  where
    gen (n, m:ms) = Assign (Entry n) (foldl Union (Exit m) (map Exit ms))

-- | exits
-- 입력
--   vertices : 노드들
--   labelMap : 번호매긴 문장들
--
-- 출력
--   Exit(n)에 대한 등식
--
-- 알고리즘
--   Skip,  Write, If, While, Assert:
--     Exit(n)에 Entry(n)을 대입
--   Assign, Read:
--     Exit(n) <- Entry(n) \ v  U  { v at n }
--   Seq:
--     Unexpected errors!

exits vertices labelMap = map gen vertices
  where
    gen n =
      case Map.lookup n labelMap of
        Just stmt -> Assign (Exit n) (rhs n stmt)
        Nothing -> error $ "exits: not found a statement indexed by: " ++ show n

    rhs n (LCSkip) = Entry n
    rhs n (LCSeq l1 l2) = error $ "exits-rhs: not expected: " ++ "LCSeq " ++ show l1 ++ " " ++ show l2
    rhs n (LCAssign v expr) = Union (Diff (Entry n) v) (Singleton v n)
    rhs n (LCRead v) = Union (Diff (Entry n) v) (Singleton v n)
    rhs n (LCWrite v) = Entry n
    rhs n (LCIf expr l1 l2) = Entry n
    rhs n (LCWhile expr l) = Entry n
    rhs n (LCAssert expr) = Entry n

-- | solve
-- 입력
--   entries 등식과 exits 등식
--
-- 출력
--   이 등식을 만족하는 최소의 해
--     Entry(n) = { ... }
--     Exit(n)  = { ... }
--
-- 알고리즘
--   초기 해를 모두 공집합으로 두고
--   주어진 등식을 적용해서 새로운 해를 구한다.
--   새로운 해가 그 전 단계에서 구한 해와 동일할 때까지 이 등식을 푸는 과정을 반복

solve equations = repUntilNoChange initSol equations
  where
    setVars = [ set | Assign set _ <- equations ]
    initSol = foldl (\m sv -> Map.insert sv Set.empty m) Map.empty setVars
    
-- |  repUntilNoChange : F(sj) =sj+1 sj == sj+1이 같을 때까지 onestep을 반복
-- 
-- |  onesetp : F를 현재 솔루션 Si를 인자로 주어 풀이해서 Si+1 구하는 함수
--                  Si+1 =  F(Si)
-- |  F의 예시
--        Assign RDEntry(1) 집합식1
--         ...
--        Assign RDEntry(6) 집합식6
--        Assign RDExit(1) 집합식1
--         ...
--        Assign RDExit(6) 집합식6

-- | 집합식의 예시
--    집합 상수 Singleton "z" 2  { (z,2) }
--    변수 Entry 3               RDEntry(3)
--    변수 Exit 2                RDExit(2)
--    Diff 집합식 "t"            eval(집합식) \ { ("t",1), ..., ("t", 6) }
--    Uniton 집합식1 집합식2      eval(집합식1) U eval(집합식2)

--
-- |  eval : 현재 솔루션 Si를 기준으로 주어진 집합 식을 풀어 어떤 집합을 리턴
--
repUntilNoChange sol equations =
--  trace (show sol) $ 
  let (solNext, isChanged) = onestep sol equations in
    if isChanged then repUntilNoChange solNext equations else solNext
  where
    onestep sol equations =
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


-- | Control-flow graph analysis

computeCfg labelMap n graph = (cfgRoot, graph')
  where
    (cfgRoot, _, graph') = cfg labelMap n graph  


-- | Control-flow graph analysis
--
-- 입력
--   1) labelMap : 번호를 매긴 문장 맵
--   2) n        : 루트 번호 (시작 번호)
--   3) graph    : 누적 제어 흐름 그래프 
--   
-- 출력
--   1) n  : 제어 흐름이 시작되는 LCSkip, LCAssign, LCRead, LCWrite, LCAssert 문장 번호
--   2) ns : n을 시작으로 제어 흐름이 끝나는 문장들 번호 ns
--   3) new graph : 업데이트된 누적 제어 흐름 그래프
--
-- 알고리즘
--   LCSkip, LCAssign, LCRead, LCWrite, LCAssert:
--      (n, [n], graph)
--
--   LCSeq n1 n2:
--      n1을 루트로 제어 흐름 그래프를 graph1으로 업데이트하고
--      실제 시작 문장 (LCSkip, LCAssign, LCRead, LCWrite, LCAssert) sn1
--      n1을 시작으로 제어 흐름이 끝나는 문장들 번호 ens1
--
--      n2를 루트로 제어 흐름 그래프를 graph2로 업데이트하고
--      실제 시작 문장 (LCSkip, LCAssign, LCRead, LCWrite, LCAssert) sn2
--      n2를 시작으로 제어 흐름이 끝나는 문장들 번호 ens2
--
--      ens1에 속한 문장 번호에서 sn2로 가는 에지를 grpah2에 추가
--      (예: n1이 if문을 가리키면 then과 else에서 n2로 가도록)
--
--      최종적으로 sn1, ens2, 업데이트된 graph2를 반환
--
--   LCIf expr nThen nElse:
--      n에서 nThen을 루트로 제어 흐름을 따라갈 때 실제 시작 문장 sn1으로 에지 추가
--      n에서 nElse를 루트로 제어 흐름을 따라갈 때 실제 시작 문장 sn2로 에지 추가
--      이렇게 만들어진 그래프를 graph0라고 놓고,
--      (참고: lazy evaluation을 사용. Python등으로 구현할때는 나중에 두 에지를 추가해도 됨)
--
--      nThen을 루트로 제어 흐름 그래프 graph0를 grpah1으로 업데이트
--      nThen을 시작으로 제어 흐름이 끝나는 문장들 번호 ens1
--
--      nElse를 루트로 제어 흐름 그래프 graph1을 grpah2로 업데이트
--      nElse를시작으로 제어 흐름이 끝나는 문장들 번호 ens2
-- 
--      최종적으로 n, ens1과 ens2 합한 리스트, graph2를 반환
--
--   LCWhile expr nBody:
--      nBody를 루트로 제어 흐름 그래프 graph를 graph1으로 업데이트
--      nBody를 루트로 제어 흐름을 따라갈 때 실제 시작 문장 sn1
--      nBody를 루트로 제어 흐름을 따라갈 때 끄타는 문장들 번호 ens1
--
--      graph1에서 n에서 sn1으로 가는 에지를 추가하고
--      이 결과 그래프에 ens1의 각 번호에 해당하는 문장들에서 n으로 가는 에지를 추가
--
--      n, [n], 이 최종 그래프를 반환

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
        LCAssert expr -> (n, [n], graph)

--
mkSeq [comm] = comm
mkSeq (comm1:comm2:comms) = mkSeq (CSeq comm1 comm2 : comms)
mkSeq [] = error $ "mkSeq: Unexpected empty list"

-- | Numbering statements
--  
-- 입력 
--   1) entry    : 번호 매김 시작 번호 
--   2) comm     : AST 문장
--   3) labelMap : { 번호 => 번호 매긴 문장 } 매핑
--  
-- 출력
--   1) root entry   : AST 문장 comm을 번호 매긴 문장으로 변환했을때
--                   가장 루트에 위치한 번호 매긴 문장의 번호
--   2) new labelMap : 업데이트된 labelMap
--
-- 알고리즘
--   CSkip, CAssign, CRead, CWrite, CAssert:
--        entry를 root entry로 하고,
--        { entry => 해당하는 번호 매긴 문장}을 labelMap에 추가
--
--   CSeq comm1 comm2:
--        entry를 시작 번호로 comm1을 넘버링하여 exit1, labelMap1을 결과로 얻음
--        exit1+1을 시작 번호로 comm2를 넘버링하여 exit2, labelMap2를 결과로 얻음
--        { (exit2+1) => LCSeq exit1 exit2 }를 labelMap2에 추가하고
--        exit2+1을 root entry로 리턴
--
--   CIf expr comm1 comm2, CWhile expr comm1:
--         CSeq의 경우와 비슷하게 진행

numbering :: Int -> Comm -> Map.Map Int LabeledComm -> (Int, Map.Map Int LabeledComm)

numbering entry CSkip labelMap = (entry, Map.insert entry LCSkip labelMap)

numbering entry (CSeq comm1 comm2) labelMap =
  let (exit1, labelMap1) = numbering entry comm1 labelMap
      (exit2, labelMap2) = numbering (exit1+1) comm2 labelMap1
  in  (exit2 + 1, Map.insert (exit2 + 1) (LCSeq exit1 exit2) labelMap2)

numbering entry (CAssign varName expr) labelMap =
  (entry, Map.insert entry (LCAssign varName expr) labelMap)

numbering entry (CRead varName) labelMap =
  (entry, Map.insert entry (LCRead varName) labelMap)

numbering entry (CWrite expr) labelMap =
  (entry, Map.insert entry (LCWrite expr) labelMap)

numbering entry (CIf expr comm1 comm2) labelMap =
  let (exit1, labelMap1) = numbering entry comm1 labelMap
      (exit2, labelMap2) = numbering (exit1+1) comm2 labelMap1
  in  (exit2+1, Map.insert (exit2+1) (LCIf expr exit1 exit2) labelMap2)

numbering entry (CWhile expr comm) labelMap =
  let (exit, labelMap1) = numbering entry comm labelMap
  in  (exit + 1, Map.insert (exit + 1) (LCWhile expr exit) labelMap1)

numbering entry (CAssert expr) labelMap =
  (entry, Map.insert entry (LCAssert expr) labelMap)
