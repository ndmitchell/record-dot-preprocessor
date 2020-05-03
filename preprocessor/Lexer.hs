{-# LANGUAGE RecordWildCards, BangPatterns #-}

-- Most of this module follows the Haskell report, https://www.haskell.org/onlinereport/lexemes.html
module Lexer(Lexeme(..), lexer, unlexerFile) where

import Data.Char
import Data.List
import Data.Tuple.Extra

-- | A lexeme of text, approx some letters followed by some space.
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
lexer = go1 1 1
    where
        -- we might start with whitespace, before any lexemes
        go1 line col xs
            | (whitespace, xs) <- lexerWhitespace xs
            , whitespace /= ""
            , (line2, col2) <- reposition line col whitespace
            = Lexeme{lexeme="", ..} : go line2 col2 xs
        go1 line col xs = go line col xs

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
lexerLexeme ('\'':x:'\'':xs) = (['\'',x,'\''], xs)
lexerLexeme ('\'':x:xs) | x /= '\'' = ("\'", x:xs) -- might be a data kind, see #25
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


unlexerFile :: FilePath -> [Lexeme] -> String
unlexerFile src xs =
    dropping 1 ++
    go 1 True [(line, lexeme ++ whitespace) | Lexeme{..} <- xs]
    where
        go
            :: Int -- ^ What line does GHC think we are on
            -> Bool -- ^ Are we at the start of a line
            -> [(Int, String)] -- ^ (original line, lexemes followed by their whitespace)
            -> String
        go ghcLine startLine ((i, x):xs) =
            (if emitDropping then dropping i else "") ++
            x ++
            go ((if emitDropping then i else ghcLine) + length (filter (== '\n') x)) ("\n" `isSuffixOf` x) xs
            where emitDropping = ghcLine /= i && i /= 0 && startLine
        go _ _ [] = ""

        -- write out a line marker with a trailing newline
        dropping n = "{-# LINE " ++ show n ++ " " ++ show src ++ " #-}\n"
