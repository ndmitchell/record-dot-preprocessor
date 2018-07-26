{-# OPTIONS_GHC -fplugin=RecordDotPlugin #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds #-}

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}


display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

-- make sure we parse applications properly
quirks c x = unwords
    [f c.name x
    ,f c.owner.name x
    ,g $ (1::Int) @@ c.name @@ True
    ,g $ 1 @@ c.owner.name @@ True
    ]

f :: String -> Bool -> String
f x _ = x

(@@) = (,)

g :: ((Int, String), Bool) -> String
g ((_,x),_) = x

main = do
    putStrLn $ display c
    putStrLn $ quirks c True
    where c = Company "A" $ Person "B" 3
