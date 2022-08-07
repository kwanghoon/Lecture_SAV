module Main where

import Expr
import Lexer (lexerSpec)
import Parser (parserSpec)
import Terminal (terminalToString)
import CommonParserUtil (lexing, parsing, endOfToken, aLexer)
import TokenInterface (fromToken)

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

  astprog <-
    parsing debugFlag                        -- parser converting a text-based program
       parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))
  
  putStrLn . show . fromASTProg $ astprog


-- The Lexer : just for printing lexical analysis results
doLexing fileName = do
  text <- readFile fileName
  let debugFlag = False
  tokens <- lexing lexerSpec () text
  mapM_ putStrLn (map terminalToString tokens)


example1 =
  CSeq
    (CAssign "x" (ECst (CInt 1)))
    (CWhile
       (EBinOp OpLessThan (EVar "x") (ECst (CInt 10)))
       (CAssign "x" (EBinOp OpAdd (EVar "x") (ECst (CInt 1)))))
