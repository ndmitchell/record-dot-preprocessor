-- Test for everything that is supported by both the plugin and the preprocessor

{-# OPTIONS_GHC -Werror -Wall -Wno-type-defaults -Wno-partial-type-signatures -Wincomplete-record-updates -Wno-unused-top-binds #-} -- can we produce -Wall clean code
{-# LANGUAGE PartialTypeSignatures, GADTs, StandaloneDeriving, DataKinds, KindSignatures, TypeFamilies #-} -- also tests we put language extensions before imports

import Data.Functor.Identity

-- ---------------------------------------------------------------------
-- Deal with HKD

type family C (f :: * -> *) (a :: *) where
  C Identity a = a
  C f a = f a

data Foo f = Foo {
  bar :: C f Int,
  baz :: String
  }


(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = if a == b then pure () else fail $ "Mismatch, " ++ show a ++ " /= " ++ show b

main :: IO ()
main = test1 >> putStrLn "All worked"

tstObj :: Foo Identity
tstObj = Foo 1 "test"

test1 :: IO ()
test1 = do
  tstObj.baz === "test"
  tstObj.bar === 1