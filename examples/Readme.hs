-- This is the example from README.md to test

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}

main :: IO ()
main = putStrLn $ display $ nameAfterOwner c
    where c = Company "A" $ Person "B" 3
