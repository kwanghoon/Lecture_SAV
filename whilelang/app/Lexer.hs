module Lexer(lexerSpec) where

import CommonParserUtil
import Token
import qualified Data.Map as Map

mkFn :: Token -> LexAction Token IO ()   -- (String -> Maybe Token)
mkFn tok = \text -> return $ Just tok

skip :: LexAction Token IO ()            -- String -> Maybe Token
skip = \text -> return Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken = TkEndOfTokens,
    lexerSpecList =
      [
        ("[ \t\n]", skip),   -- skip a space or a tab, not a return!
        
        ("\'[^\n]*" , skip),  -- comment starting with '

        ("-?[0-9]+" , mkFn TkCInt),

        -- TkCBool [OK]

        -- TkTyInt [OK]
        -- TkTyBool [OK]

        ("\\+", mkFn TkAdd),
        ("-", mkFn TkSub),
        ("\\*", mkFn TkMul),
        ("/", mkFn TkDiv),
        ("\\%", mkFn TkMod),

        -- TkLessThan [OK]
        -- TkGreaterThan [OK]
        ("<=", mkFn TkLessThanEqualTo),
        ("<",  mkFn TkLessThan),

        (">=", mkFn TkGreaterThanEqualTo),
        (">",  mkFn TkGreaterThan),

        ("==", mkFn TkEqualTo),
        ("!=", mkFn TkNotEqualTo),

        ("&&", mkFn TkAnd),
        ("\\|\\|", mkFn TkOr),
        ("!", mkFn TkNot),

        -- TkSkip [OK]
        (";", mkFn TkSemicolon),
        ("=", mkFn TkAssign),
        
        ("{", mkFn TkOpenBrace),
        ("}", mkFn TkCloseBrace),
        ("\\(", mkFn TkOpenParen),
        ("\\)", mkFn TkCloseParen),

        ("[_a-zA-Z][a-zA-Z0-9]*", keywordOrIdentifier)
      ]
  }

keywordOrIdentifier text =
  case Map.lookup text keywords of
    Nothing  -> mkFn TkIdentifier text
    Just tok -> mkFn tok text

keywords :: Map.Map String Token
keywords =
  Map.fromList
    [ ("true",   TkCBool)
    , ("false",  TkCBool)

    , ("int",    TkTyInt)
    , ("bool",   TkTyBool)

    , ("skip",   TkSkip)
    , ("read",   TkRead)
    , ("write",  TkWrite)
    
    , ("if",     TkIf)
    , ("then",   TkThen)
    , ("else",   TkElse)
    , ("while",  TkWhile)
    
    , ("assert",  TkAssert)
    ]
