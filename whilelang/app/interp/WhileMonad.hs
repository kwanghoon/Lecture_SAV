module WhileMonad where

-- Operations for While
class Monad m => WhileMonad m where
  readInt     :: m Int
  readBool    :: m Bool
  writeString :: String -> m ()


