{-# OPTIONS_GHC -Werror -Wall -Wno-type-defaults #-} -- can we produce -Wall clean code
{-# LANGUAGE ScopedTypeVariables #-}

-- can you deal with modules and existing extensions
module Main(main) where

import Control.Exception

fails :: a -> IO ()
fails x = do
    res <- try $ evaluate x
    case res of
        Left (_ :: SomeException) -> return ()
        Right _ -> fail "Expected an exception"

(===) :: Eq a => a -> a -> IO ()
a === b = if a == b then return () else fail "Mismatch"


-- can you deal with multiple alternatives
data Animal = Human {name :: String, job :: String}
            | Nonhuman {name :: String}
              deriving (Show,Eq)

test1 :: IO ()
test1 = do
    (Human "a" "b").name === "a" -- comment here
    (Nonhuman "x").name === "x"
    fails (Nonhuman "x").job

-- can you deal with polymorphism
data Foo a b = Foo {name :: (a, Maybe b), the_b :: b}
    deriving Eq

test2 :: IO ()
test2 = do
    let foo1 = Foo{name=(1, Nothing), the_b=Human "a" "b"}
    let foo2 = Foo (19, Just 2) 2
    foo1.the_b.job === "b"
    foo2.name._1 === 19

    foo2{the_b = 8}.the_b === 8
    -- foo1.the_b{job & toUpper} == foo{the_b.job = "B"}


main :: IO ()
main = test1 >> test2
