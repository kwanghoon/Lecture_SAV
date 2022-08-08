module Interp where

import Expr
import qualified Data.Map as Map
import WhileMonad

interp :: Expr -> WhileMonad ()
interp expr = return ()

--
type Env = Map String Const




