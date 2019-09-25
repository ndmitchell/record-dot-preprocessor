-- Test for everything that is supported by both the plugin and the preprocessor

{-# OPTIONS_GHC -Werror -Wall -Wno-type-defaults -Wno-partial-type-signatures #-} -- can we produce -Wall clean code
{-# LANGUAGE PartialTypeSignatures, GADTs, StandaloneDeriving #-} -- also tests we put language extensions before imports

import Control.Exception

main :: IO ()
main = test1 >> test2 >> test3 >> putStrLn "All worked"

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = if a == b then return () else fail $ "Mismatch, " ++ show a ++ " /= " ++ show b

fails :: a -> IO ()
fails val = do
    res <- try $ evaluate val
    case res of
        Left e -> let _ = e :: SomeException in return ()
        Right _ -> fail "Expected an exception"


---------------------------------------------------------------------
-- CHECK THE BASICS WORK

data Foo a = Foo {foo1 :: !a, _foo2 :: Int} deriving (Show,Eq)

-- can you deal with multiple alternatives
data Animal = Human {name :: !String, job :: Prelude.String}
            | Nonhuman {name :: String}
              deriving (Show,Eq)


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

    -- alternatives work
    (Human "a" "b").name === "a" -- comment here
    (Nonhuman "x").name === "x"
    fails (Nonhuman "x").job


---------------------------------------------------------------------
-- DEAL WITH INFIX APPLICATIONS AND ASSOCIATIVITY

data Company = Company {name :: String, owner :: Person -- trailing comment
    }
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


---------------------------------------------------------------------
-- GADTS AND EXISTENTIALS

data GADT where
    GADT :: {gadt :: Int} -> GADT
    deriving (Show,Eq)

data V3 a = Num a => V3 { xx, yy, zz :: a }
deriving instance Show a => Show (V3 a)
deriving instance Eq a => Eq (V3 a)

test3 :: IO ()
test3 = do
    let val = GADT 3
    val.gadt === 3
    val{gadt=5} === GADT 5

    let v3 = V3 1 2 3
    v3.xx === 1
    v3{yy=1, zz=2} === V3 1 1 2
