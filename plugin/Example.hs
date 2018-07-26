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
    ,gL $ 1 @+ c.name @+ True
    ,gL $ 1 @+ c.owner.name @+ True
    ,gL $ 1 @+ f c.name x @+ True
    ,gL $ 1 @+ f c.owner.name x @+ True
    ,gR $ 1 +@ c.name +@ True
    ,gR $ 1 +@ c.owner.name +@ True
    ,gR $ 1 +@ f c.name x +@ True
    ,gR $ 1 +@ f c.owner.name x +@ True
    ]

f :: String -> Bool -> String
f x _ = x

infixl 9 @+
infixr 9 +@
(@+) = (,)
(+@) = (,)

gL :: ((Int, String), Bool) -> String
gL ((_,x),_) = x

gR :: (Int, (String, Bool)) -> String
gR (_,(x,_)) = x

main = do
    putStrLn $ display c
    putStrLn $ quirks c True
    where c = Company "A" $ Person "B" 3
