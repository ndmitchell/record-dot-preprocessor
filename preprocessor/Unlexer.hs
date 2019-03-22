{-# LANGUAGE RecordWildCards #-}

-- Most of this module follows the Haskell report, https://www.haskell.org/onlinereport/lexemes.html
module Unlexer(unlexer) where

import Lexer
import Data.List

unlexer :: FilePath -> [Lexeme] -> String
unlexer src xs =
    dropping 1 ++
    go 1 True [(line, lexeme ++ whitespace) | Lexeme{..} <- xs]
    where
        go :: Int -> Bool -> [(Int, String)] -> String
        go doc drp ((i, x):xs) =
            (if doc /= i && i /= 0 && drp then dropping i else "") ++
            x ++
            go ((if i == 0 then doc else i) + length (filter (== '\n') x)) ("\n" `isSuffixOf` x) xs
        go _ _ [] = ""

        dropping n = "{-# LINE " ++ show n ++ " " ++ show src ++ " #-}\n"
