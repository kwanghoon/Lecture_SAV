module Token where

import TokenInterface

data Token =
    TkEndOfTokens
    
  | TkCInt         -- 1, 2, 3
  | TkCBool        -- true, false

  | TkIdentifier   -- x, y, abc123, _z

  | TkTyInt        -- int
  | TkTyBool       -- bool

  | TkAdd                 -- +
  | TkSub                 -- -
  | TkMul                 -- *
  | TkDiv                 -- /
  | TkMod                 -- %

  | TkLessThan            -- <
  | TkGreaterThan         -- >
  | TkLessThanEqualTo     -- <=
  | TkGreaterThanEqualTo  -- >=
  | TkEqualTo             -- ==
  | TkNotEqualTo          -- !=

  | TkAnd                 -- &&
  | TkOr                  -- ||
  | TkNot                 -- !

  | TkSkip                -- skip   (keyword)
  | TkSemicolon           -- ;
  | TkAssign              -- =
  | TkRead                -- read
  | TkWrite               -- write
  | TkIf                  -- if
  | TkThen                -- then
  | TkElse                -- else
  | TkWhile               -- while
  | TkOpenBrace           -- {
  | TkCloseBrace          -- }
  | TkOpenParen           -- (
  | TkCloseParen          -- )
  
  | TkAssert              -- assert
  deriving (Eq,Show)



-- For lexical analysis

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (TkEndOfTokens,"$")
  
  , (TkCInt, "TkCInt")
  , (TkCBool, "TkCBool")
  
  , (TkIdentifier, "TkIdentifier")
  
  , (TkTyInt, "int")
  , (TkTyBool, "bool")

  , (TkAdd, "+")
  , (TkSub, "-")
  , (TkMul, "*")
  , (TkDiv, "/")
  , (TkMod, "%")

  , (TkLessThan, "<")
  , (TkGreaterThan, ">")
  , (TkLessThanEqualTo, "<=")
  , (TkGreaterThanEqualTo, ">=")
  , (TkEqualTo, "==")
  , (TkNotEqualTo, "!=")
  
  , (TkAnd, "&&")
  , (TkOr, "||")
  , (TkNot, "!")
  
  , (TkSkip, "skip")
  , (TkSemicolon, ";")
  , (TkAssign, "=")
  , (TkRead, "read")
  , (TkWrite, "write")
  , (TkIf, "if")
  , (TkThen, "then")
  , (TkElse, "else")
  , (TkWhile, "while")
  , (TkOpenBrace, "{")
  , (TkCloseBrace, "}")
  , (TkOpenParen, "(")
  , (TkCloseParen, ")")
  
  , (TkAssert, "assert")
  ]

findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

instance TokenInterface Token where
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
      
  isEOT TkEndOfTokens = True
  isEOT _             = False
