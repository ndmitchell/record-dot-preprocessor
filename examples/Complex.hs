{-# OPTIONS_GHC -Werror -Wall -Wno-type-defaults #-} -- can we produce -Wall clean code
{-# LANGUAGE ScopedTypeVariables #-}

-- can you deal with modules and existing extensions
module Main(main) where

import Control.Exception

fails :: a -> IO ()
fails val = do
    res <- try $ evaluate val
    case res of
        Left (_ :: SomeException) -> return ()
        Right _ -> fail "Expected an exception"

(===) :: Eq a => a -> a -> IO ()
a === b = if a == b then return () else fail "Mismatch"


-- can you deal with multiple alternatives
data Animal = Human {name :: !String, job :: Prelude.String}
            | Nonhuman {name :: String}
              deriving (Show,Eq)

test1 :: IO ()
test1 = do
    (Human "a" "b").name === "a" -- comment here
    (Nonhuman "x").name === "x"
    fails (Nonhuman "x").job

-- can you deal with polymorphism
data Foo a b = Foo {name :: (a, Maybe b), the_b :: b, x :: Int}
    deriving Eq

data Person = Person {age :: Int, address :: String}
    deriving Eq

test2 :: IO ()
test2 = do
    let foo1 = Foo{name=(1, Nothing), the_b=Human "a" "b", x=1}
    let foo2 = Foo (19, Just 2) 2 1
    foo1.the_b.job === "b"
    foo2.name._1 === 19
    foo2.x === 1

    -- check complex updates
    foo2{the_b = 8}.the_b === 8
    foo1{the_b.job = "c"} === foo1{the_b = foo1.the_b{job = "c"}}
    foo1.the_b{job ++ "b"} === (foo1.the_b){job = "bb"}
    foo1{the_b.job ++ "b", the_b.name = "q"} === foo1{the_b = Human "q" "bb"}

    -- check updates are ordered correctly
    foo1{the_b = Human "x" "y", the_b.job="z"} === foo1{the_b = Human "x" "z"}

    -- check for nesting
    (foo1.the_b).job === "b"
    foo1{the_b = foo1.the_b{job="r"}}.the_b.job === "r"
    (foo1{the_b.job="n"}){the_b.name="m"}.the_b === Human "m" "n"
    let foo11 = (foo1, foo1)
    foo11._1.the_b{job="n"} === Human "m" "n"

    -- check we don't go into constructors
    Control.Exception.evaluate ()

    let person = Person 10 "Home"
    (person{age - 3}){age * 2} === person{age = 14}


main :: IO ()
main = test1 >> test2 >> putStrLn "Complex example worked"
