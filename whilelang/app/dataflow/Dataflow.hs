module Dataflow where

import qualified Data.Map as Map

import Expr

type Label = Int

data LabeledComm =
    LCSkip   
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
    (_, map) = numberingProg Map.empty 1 comms
    
    decls = progDecls prog
    comms = progComms prog

numberingProg map entry []           = (entry, map)
numberingProg map entry (comm:comms) =
  let (exit, map1) = numbering entry comm map in
    numberingProg map1 exit comms


--
numbering :: Int -> Comm -> Map.Map Int LabeledComm -> (Int, Map.Map Int LabeledComm)

numbering entry CSkip map = (entry+1, Map.insert entry LCSkip map)

numbering entry (CSeq comm1 comm2) map =
  let (exit1, map1) = numbering entry comm1 map
      (exit2, map2) = numbering exit1 comm2 map1
  in  (exit2, map2)

numbering entry (CAssign varName expr) map =
  (entry+1, Map.insert entry (LCAssign varName expr) map)

numbering entry (CRead varName) map =
  (entry+1, Map.insert entry (LCRead varName) map)

numbering entry (CWrite expr) map =
  (entry+1, Map.insert entry (LCWrite expr) map)

numbering entry (CIf expr comm1 comm2) map =
  let (exit1, map1) = numbering (entry+1) comm1 map
      (exit2, map2) = numbering exit1 comm2 map1
  in  (exit2, Map.insert entry (LCIf expr (entry+1) exit1) map2)

numbering entry (CWhile expr comm) map =
  let (exit, map1) = numbering (entry+1) comm map
  in  (exit, Map.insert entry (LCWhile expr (entry+1)) map1)