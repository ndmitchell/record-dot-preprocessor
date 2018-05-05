{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}

main = putStrLn $ display $ nameAfterOwner c
    where c = Company "A" $ Person "B" 3
