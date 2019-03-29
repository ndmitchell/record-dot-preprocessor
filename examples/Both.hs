-- Test for everything that is supported by both the plugin and the preprocessor

{-# OPTIONS_GHC -Werror -Wall -Wno-type-defaults -Wno-partial-type-signatures #-} -- can we produce -Wall clean code
{-# LANGUAGE PartialTypeSignatures #-} -- also tests we put language extensions before imports

main :: IO ()
main = test1 >> test2 >> putStrLn "Both worked"

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = if a == b then return () else fail $ "Mismatch, " ++ show a ++ " /= " ++ show b


---------------------------------------------------------------------
-- CHECK THE BASICS WORK

data Foo a = Foo {foo1 :: !a, _foo2 :: Int} deriving (Show,Eq)

test1 :: IO ()
test1 = do
    -- test expr.lbl
    (Foo "test" 1).foo1 === "test"
    let foo = Foo "test" 2
    foo.foo1 === "test"
    foo._foo2 === 2
    (Foo (1,2) 3).foo1._1 === 1
    let foo2 = Foo (1,2) 3
    foo2.foo1._2 === 2
    (foo2.foo1)._2 === 2

    -- test expr{lbl = val}
    foo {foo1 = "a"} === Foo "a" 2
    foo {foo1 = "a", foo1 = "b"} === foo{foo1 = "b"}
    null (foo{foo1 = []}.foo1) === True
    foo{foo1 = "a"}.foo1 === "a"
    let _foo2 = 8 in foo{_foo2} === Foo "test" 8

    -- (.lbl)
    map (.foo1) [foo, foo{foo1="q"}] === ["test", "q"]
    ( .foo1._foo2 ) (Foo foo 3) === 2


---------------------------------------------------------------------
-- DEAL WITH INFIX APPLICATIONS AND ASSOCIATIVITY

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

test2 :: IO ()
test2 = do
    let c = Company "A" $ Person "B" 3
    let x = True
    (===) "A" $ f c.name x
    (===) "B" $ f c.owner.name x
    (===) "A" $ gL $ 1 @+ c.name @+ True
    (===) "B" $ gL $ 1 @+ c.owner.name @+ True
    (===) "A" $ gL $ 1 @+ f c.name x @+ True
    (===) "B" $ gL $ 1 @+ f c.owner.name x @+ True
    (===) "A" $ gR $ 1 +@ c.name +@ True
    (===) "B" $ gR $ 1 +@ c.owner.name +@ True
    (===) "A" $ gR $ 1 +@ f c.name x +@ True
    (===) "B" $ gR $ 1 +@ f c.owner.name x +@ True

f :: String -> Bool -> String
f x _ = x

infixl 9 @+
infixr 9 +@
(@+), (+@) :: _
(@+) = (,)
(+@) = (,)

gL :: ((Int, String), Bool) -> String
gL ((_,x),_) = x

gR :: (Int, (String, Bool)) -> String
gR (_,(x,_)) = x
