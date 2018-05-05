{-# LANGUAGE RecordWildCards #-}

-- Most of this module follows the Haskell report, https://www.haskell.org/onlinereport/lexemes.html
module Unlexer(unlexer) where

import Lexer

unlexer :: FilePath -> [Lexeme] -> String
unlexer _ xs = concat [whitespace ++ lexeme | Lexeme{..} <- xs]
