module WhileMonad where

-- Operations for While
class Monad m => WhileMonad m where
  readInt     :: m Int
  readString  :: m String
  writeInt    :: Int -> m ()
  writeString :: String -> m ()

