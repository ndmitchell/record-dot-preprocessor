{-# LANGUAGE RecordWildCards, BangPatterns #-}

-- Most of this module follows the Haskell report, https://www.haskell.org/onlinereport/lexemes.html
module Lexer(Lexeme(..), lexer) where

import Data.Char
import Data.Tuple.Extra

data Lexeme = Lexeme
    {line :: {-# UNPACK #-} !Int -- ^ 1-based line number (0 = generated)
    ,col :: {-# UNPACK #-} !Int -- ^ 1-based col number (0 = generated)
    ,lexeme :: String -- ^ Actual text of the item
    ,whitespace :: String -- ^ Suffix spaces and comments
    } deriving Show


charNewline x = x == '\r' || x == '\n' || x == '\f'
charSpecial x = x `elem` "(),;[]`{}"
charAscSymbol x = x `elem` "!#$%&*+./<=>?@\\^|-~" || x == ':' -- special case for me
charSymbol x = charAscSymbol x || (isSymbol x && not (charSpecial x) && x `notElem` "_\"\'")

charIdentStart x = isAlpha x || x == '_'
charIdentCont x = isAlphaNum x || x == '_' || x == '\''


lexer :: String -> [Lexeme]
lexer = go 1 1
    where
        go line col "" = []
        go line col xs
            | (lexeme, xs) <- lexerLexeme xs
            , (whitespace, xs) <- lexerWhitespace xs
            , (line2, col2) <- reposition line col $ whitespace ++ lexeme
            = Lexeme{..} : go line2 col2 xs


reposition :: Int -> Int -> String -> (Int, Int)
reposition = go
    where
        go !line !col [] = (line, col)
        go line col (x:xs)
            | x == '\n' = go (line+1) 1 xs
            | x == '\t' = go line (col+8) xs -- technically not totally correct, but please, don't use tabs
            | otherwise = go line (col+1) xs


-- We take a lot of liberties with lexemes around module qualification, because we want to make fields magic
-- we ignore numbers entirely because they don't have any impact on what we want to do
lexerLexeme :: String -> (String, String)
lexerLexeme (open:xs) | open == '\'' || open == '\"' = seen [open] $ go xs
    where
        go (x:xs) | x == open = ([x], xs)
                  | x == '\\', x2:xs <- xs = seen [x,x2] $ go xs
                  | otherwise = seen [x] $ go xs
        go [] = ([], [])
lexerLexeme (x:xs)
    | charSymbol x
    , (a, xs) <- span charSymbol xs
    = (x:a, xs)
lexerLexeme (x:xs)
    | charIdentStart x
    , (a, xs) <- span charIdentCont xs
    = (x:a, xs)
lexerLexeme (x:xs) = ([x], xs)
lexerLexeme [] = ([], [])


lexerWhitespace :: String -> (String, String)
lexerWhitespace (x:xs) | isSpace x = seen [x] $ lexerWhitespace xs
lexerWhitespace ('-':'-':xs)
    | (a, xs) <- span (== '-') xs
    , not $ any charSymbol $ take 1 xs
    , (b, xs) <- break charNewline xs
    , (c, xs) <- splitAt 1 xs
    = seen "--" $ seen a $ seen b $ seen c $ lexerWhitespace xs
lexerWhitespace ('{':'-':xs) = seen "{-" $ f 1 xs
    where
        f 1 ('-':'}':xs) = seen "-}" $ lexerWhitespace xs
        f i ('{':'-':xs) = seen "{-" $ f (i+1) xs
        f i (x:xs) = seen [x] $ f i xs
        f i [] = ([], [])
lexerWhitespace xs = ([], xs)

seen xs = first (xs++)
