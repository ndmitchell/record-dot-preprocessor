-- Test DuplicateRecordFields extension

main :: IO ()
main = test1 >> putStrLn "All worked"

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = if a == b then pure () else fail $ "Mismatch, " ++ show a ++ " /= " ++ show b


---------------------------------------------------------------------
-- CHECK DUPLICATE NAMES WORK

data Foo = Foo {id :: String} deriving (Show, Eq)

test1 :: IO ()
test1 = do
    (Foo "test").id === "test"
    (Foo "test"){id = "bar"} === Foo "bar"
    map (.id) [Foo "test"] === ["test"]
