module Main where

import Expr

main :: IO ()
main = putStrLn $ show $ example1

example1 =
  CSeq
    (CAssign "x" (ECst (CInt 1)))
    (CWhile
       (EBinOp BLessThan (EVar "x") (ECst (CInt 10)))
       (CAssign "x" (EBinOp BAdd (EVar "x") (ECst (CInt 1)))))
