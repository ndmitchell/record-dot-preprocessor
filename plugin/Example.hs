{-# OPTIONS_GHC -fplugin=RecordDotPlugin #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, DataKinds #-}

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

main = putStrLn $ display c
    where c = Company "A" $ Person "B" 3
