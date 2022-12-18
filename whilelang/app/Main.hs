{-# LANGUAGE LambdaCase     #-}

module Main where

import Expr
import Util

import System.IO
import System.Environment (getArgs, withArgs)


main :: IO ()
main =
 do args <- getArgs
    _main args

_main [] = return ()
_main (fileName:args) = 
  case fileName of
    _ -> do _ <- doRun {- True -} fileName
            _main args

example1 =
  Prog
  { progDecls = [],
    progComms =
    [
      CSeq
        (CAssign "x" (ECst (CInt 1)))
        (CWhile
           (EBinOp OpLessThan (EVar "x") (ECst (CInt 10)))
           (CAssign "x" (EBinOp OpAdd (EVar "x") (ECst (CInt 1)))))
    ]
  }
