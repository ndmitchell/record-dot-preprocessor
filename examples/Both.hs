-- Test for everything that is supported by both the plugin and the preprocessor

{-# OPTIONS_GHC -Werror -Wall -Wno-type-defaults -Wno-partial-type-signatures #-} -- can we produce -Wall clean code
{-# LANGUAGE PartialTypeSignatures, GADTs, StandaloneDeriving, DataKinds #-} -- also tests we put language extensions before imports

import Control.Exception
import Data.Version

main :: IO ()
main = test1 >> test2 >> test3 >> test4 >> test5 >> putStrLn "All worked"

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = if a == b then pure () else fail $ "Mismatch, " ++ show a ++ " /= " ++ show b

fails :: a -> IO ()
fails val = do
    res <- try $ evaluate val
    case res of
        Left e -> let _ = e :: SomeException in pure ()
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
    foo{foo1 = "a"} === Foo "a" 2
    foo{foo1 = "a", foo1 = "b"} === foo{foo1 = "b"}
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


type Type = '[Int]


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

-- ---------------------------------------------------------------------
-- Another volley of tests combining constructions, updates and
-- applications adapted from the DAML test-suite

data AA = AA {xx :: Int} deriving (Eq, Show)
data BB = BB {yy :: AA, zz :: AA} deriving (Eq, Show)
data CC = CC {aa :: Int, bb :: Int} deriving (Eq, Show)

test4 :: IO ()
test4 = do
  f1 CC{aa = 1, bb = 2} 3 4 === CC{aa = 3, bb = 4}
  f2 CC{aa = 1, bb = 2} 1 2 === CC{aa = 3, bb = 2}
  (f3 AA{xx = 1}).xx === 2
  (f4 BB{yy = AA{xx = 1}, zz = AA{xx = 2}}).zz.xx === 4
  let res = f4 BB{yy = AA{xx = 1}, zz = AA{xx = 2}} in res.zz.xx === 4
  (f5 BB{yy = AA{xx = 1}, zz = AA{xx = 2}}).zz.xx === 4
  (f6 BB{yy = AA{xx = 1}, zz = AA{xx = 2}}).yy.xx === 2
  (f6 BB{yy = AA{xx = 1}, zz = AA{xx = 2}}).zz.xx === 4
  f7 [AA 1, AA 2, AA 3] === [1, 2, 3]
  f8 [BB (AA 1) (AA 2), BB (AA 2) (AA 3), BB (AA 3) (AA 4)] === [1, 2, 3]
  where
    f1 :: CC -> Int -> Int -> CC; f1 s t u = s{aa = t, bb = u}
    f2 :: CC -> Int -> Int -> CC; f2 s t u = s{aa = t + u}
    f3 :: AA -> AA; f3 s = s{xx = s.xx + 1}
    f4 :: BB -> BB; f4 s = s{yy = s.yy, zz = s.zz{xx = 4}}
    f5 :: BB -> BB; f5 s = s{yy = s.yy, zz = s.zz{xx = (\ x -> x * x) s.zz.xx}}
    f6 :: BB -> BB; f6 s = s{yy = s.yy{xx = s.yy.xx + 1}, zz = s.zz{xx = (\ x -> x * x) s.zz{xx = s.zz.xx}.xx}}
    f7 :: [AA] -> [Int]; f7 l = map (.xx) l
    f8 :: [BB] -> [Int]; f8 l = map (.yy.xx) l

-- ---------------------------------------------------------------------
-- Test we can still non-instance fields

test5 :: IO ()
test5 = do
    let v = makeVersion [1,2,3]
    versionBranch v === [1,2,3]
    -- the space before the { stops it from using record update
    showVersion (v {versionBranch=[1]}) === "1"
