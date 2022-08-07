module Main where

import Expr
import Lexer (lexerSpec)
import Terminal (terminalToString)
import CommonParserUtil (lexing)

import System.IO
import System.Environment (getArgs, withArgs)

main :: IO ()
main =
 do args <- getArgs
    _main args

_main [] = return ()
_main (fileName:args) = 
  case fileName of
    _ -> do _ <- doProcess True fileName
            _main args


doProcess verbose fileName = do
  text <- readFile fileName
  let debugFlag = False
  tokens <- lexing lexerSpec () text
  mapM_ putStrLn (map terminalToString tokens)


-- The Lexer 
doLexing fileName = do
  text <- readFile fileName
  let debugFlag = False
  tokens <- lexing lexerSpec () text
  mapM_ putStrLn (map terminalToString tokens)

  -- expression <-
  --   parsing debugFlag                        -- parser converting a text-based program
  --      parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
  --      (aLexer lexerSpec)
  --      (fromToken (endOfToken lexerSpec))

example1 =
  CSeq
    (CAssign "x" (ECst (CInt 1)))
    (CWhile
       (EBinOp BLessThan (EVar "x") (ECst (CInt 10)))
       (CAssign "x" (EBinOp BAdd (EVar "x") (ECst (CInt 1)))))
