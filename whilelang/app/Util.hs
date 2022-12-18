module Util where

import Expr
import Lexer (lexerSpec)
import Parser (parserSpec)
import Terminal (terminalToString)
import CommonParserUtil (lexing, parsing, endOfToken, aLexer)
import TokenInterface (fromToken)
import WhileMonad
import Interp (interp)
import Typecheck (typecheck)
import Dataflow (dataflow, printLabeledComm)
import SymExec

import Data.Char (isDigit)

doRun fileName = do
  let verbose = False
  text <- readFile fileName
  let debugFlag = False

  astprog <-
    parsing debugFlag                        -- parser converting a text-based program
       parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

  let prog = fromASTProg astprog
  
  putStrLn . show $ prog

  typecheck prog

  interp prog


-- The Lexer : just for printing lexical analysis results
doLexing fileName = do
  text <- readFile fileName
  let debugFlag = False
  tokens <- lexing lexerSpec () text
  mapM_ putStrLn (map terminalToString tokens)

-- The parser
doParsing fileName = do
  text <- readFile fileName
  let debugFlag = False

  astprog <-
    parsing debugFlag                        -- parser converting a text-based program
       parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

  let prog = fromASTProg astprog
  
  putStrLn . show $ prog

-- The type checker
doTypecheck fileName = do
  let verbose = False
  text <- readFile fileName
  let debugFlag = False

  astprog <-
    parsing debugFlag                        -- parser converting a text-based program
       parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

  let prog = fromASTProg astprog
  
  putStrLn . show $ prog

  typecheck prog
  
-- The data flow analyzer
doAnalysis fileName = do 
  let verbose = False
  text <- readFile fileName
  let debugFlag = False

  astprog <-
    parsing debugFlag                        -- parser converting a text-based program
       parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

  let prog = fromASTProg astprog
  
  putStrLn . show $ prog

  typecheck prog

  labeledprog <- dataflow True prog
  
  return ()
  
  -- putStrLn . show $ labeledprog
  

doSymbolic fileName = do
  let verbose = False
  text <- readFile fileName
  let debugFlag = False

  astprog <-
    parsing debugFlag                        -- parser converting a text-based program
       parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

  let prog = fromASTProg astprog

  putStrLn . show $ prog

  typecheck prog
  
  symExec prog

--
--
instance WhileMonad IO where
  readInt = do w <- readWord
               if and (map isDigit w)
                 then return (read w :: Int)
                 else error $ "readInt: Unknonw integer: " ++ w

  readBool = do w <- readWord
                if w == show (CBool True)
                  then return True
                  else if w == show (CBool False)
                       then return False
                       else error $ "readBool: Unknown boolean: " ++ w
                            
  writeString s = putStr s

readWord :: IO String
readWord = do w <- readWord' $ ""
              return . reverse $ w
  where readWord' w =
          do c <- getChar
             case c of
               '\n' -> return w
               '\r' -> return w
               ' '  -> return w
               '\t' -> return w
               _    -> readWord' (c:w)
