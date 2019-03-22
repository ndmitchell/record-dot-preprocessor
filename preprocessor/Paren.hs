{-# LANGUAGE ScopedTypeVariables, DeriveFunctor #-}

-- Most of this module follows the Haskell report, https://www.haskell.org/onlinereport/lexemes.html
module Paren(Paren(..), parenOn, unparen, unparens) where

import Data.Tuple.Extra

-- | A list of items which are paranthesised.
data Paren a
    = Item a -- Indiviaaul item
    | Paren a [Paren a] a -- parenthesise, open, inner, close
    deriving (Show,Eq,Functor)

parenOn :: forall a b . Eq b => (a -> b) -> [(b, b)] -> [a] -> [Paren a]
parenOn proj pairs = fst . go Nothing
    where
        -- invariant: if first argument is Nothing, second component of result will be Nothing
        go :: Maybe b -> [a] -> ([Paren a], Maybe (a, [a]))
        go (Just close) (x:xs) | close == proj x = ([], Just (x, xs))
        go close (start:xs)
            | Just end <- lookup (proj start) pairs
            , (inner, res) <- go (Just end) xs
            = case res of
                Nothing -> (Item start : inner, Nothing)
                Just (end, xs) -> first (Paren start inner end :) $ go close xs
        go close (x:xs) = first (Item x :) $ go close xs
        go close [] = ([], Nothing)


unparens :: [Paren a] -> [a]
unparens = concatMap unparen

unparen :: Paren a -> [a]
unparen (Item x) = [x]
unparen (Paren a b c) = [a] ++ unparens b ++ [c]
