module Main where

import Expr
import Lexer (lexerSpec)
import Parser (parserSpec)
import Terminal (terminalToString)
import CommonParserUtil (lexing, parsing, endOfToken, aLexer)
import TokenInterface (fromToken)
import WhileMonad
import Interp (interp)
import Typecheck (typecheck)
import Data.Char (isDigit)
import System.IO
import System.Environment (getArgs, withArgs)

main :: IO ()
main =
 do args <- getArgs
    _main args

_main [] = return ()
_main (fileName:args) = 
  case fileName of
    _ -> do _ <- doRun True fileName
            _main args


doTypecheck verbose fileName = do
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
  
doRun verbose fileName = do
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


example1 =
  CSeq
    (CAssign "x" (ECst (CInt 1)))
    (CWhile
       (EBinOp OpLessThan (EVar "x") (ECst (CInt 10)))
       (CAssign "x" (EBinOp OpAdd (EVar "x") (ECst (CInt 1)))))

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
