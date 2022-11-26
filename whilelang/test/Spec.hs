module Main where

import Util

files =
  [ "assign.while"
--  , "bad.while"
--  , "bad1.while"
--  , "bad2.while"
--  , "bad3.while"
--  , "bad4.while"
--  , "bad5.while"
  , "factorial.while"
  , "ifcond1.while"
--  , "ifcond1_nottypechecked.while"
  , "ifcond2.while"
  , "sequence.while"
  , "while1.while"
  , "while2.while"
  , "foobar.while"
  ]

base = "./example/"

paths = [ base ++ file | file <- files]

main :: IO ()
main =
 do mapM_ doParsing paths
    mapM_ doTypecheck paths
    mapM_ doAnalysis paths
    mapM_ doSymbolic paths
    

testParsing :: [String] -> IO ()
testParsing fileNames = mapM_ doParsing fileNames
