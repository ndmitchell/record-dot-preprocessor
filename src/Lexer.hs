{-# LANGUAGE RecordWildCards #-}

module Lexer(Lexeme(..), lexer, unlexer) where

data Lexeme = Lexeme
    {line :: {-# UNPACK #-} !Int -- ^ 1-based line number
    ,col :: {-# UNPACK #-} !Int -- ^ 1-based col number
    ,space :: String -- ^ Prefix spaces and comments
    ,text :: String -- ^ Actual text of the item
    }


lexer :: String -> [Lexeme]
lexer x = [Lexeme 1 1 "" x]


unlexer :: FilePath -> [Lexeme] -> String
unlexer _ xs = concat [space ++ text | Lexeme{..} <- xs]
