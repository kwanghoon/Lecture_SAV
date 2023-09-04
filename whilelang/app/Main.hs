{-# LANGUAGE LambdaCase     #-}

module Main where

import Expr
import Util

import System.IO
import System.Environment (getArgs, withArgs)


main :: IO ()
main =
 do args <- getArgs
    _main doRun args

_main f [] = return ()
_main f (arg:args) =
  if length arg >= 2 && take 2 arg == "--" then
    changeFun f arg args
  else
    let fileName = arg in 
      do _ <- f {- True -} fileName
         _main f args

changeFun f arg args = 
 case [ newF | (opt, newF) <- cmdList, opt == arg ] of
   [] -> _main f args
   (newF:_) -> _main newF args

cmdList =
 [
   ("--lex", doLexing),
   ("--parse", doParsing),
   ("--typecheck", doTypecheck),
   ("--dataflow", doAnalysis),
   ("--symexec", doSymbolic),
   ("--json", doJson)
 ]

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
