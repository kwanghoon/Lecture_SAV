module Dataflow where

import qualified Data.Map as Map

import Expr

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
dataflow :: Prog -> IO [(Int, LabeledComm)]
dataflow prog = return $ Map.toList map
  where
    (_, map) = numbering 1 (mkSeq comms) Map.empty
    
    decls = progDecls prog
    comms = progComms prog

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