{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Edit(edit) where

import Lexer
import Paren
import Data.Maybe
import Data.Char
import Data.List.Extra
import Data.Tuple.Extra
import Control.Monad.Extra


edit :: [Paren Lexeme] -> [Paren Lexeme]
edit = editAddPreamble . editAddInstances . editSelectors . editUpdates


---------------------------------------------------------------------
-- HELPERS

pattern PL :: String -> Paren Lexeme
pattern PL x <- (unPL -> Just x) where
    PL x = Item $ L x

isPL :: String -> Paren Lexeme -> Bool
isPL x y = unPL y == Just x

unPL :: Paren Lexeme -> Maybe String
unPL (Item (L x)) = Just x
unPL _ = Nothing

pattern L :: String -> Lexeme
pattern L x <- (unL -> x) where
    L x = Lexeme 0 0 x ""

unL :: Lexeme -> String
unL = lexeme




nl = Item $ Lexeme 0 0 "" "\n"
spc = Item $ Lexeme 0 0 "" " "
gen = Item . gen_
gen_ x = Lexeme 0 0 x ""

paren [x] = x
paren xs = case unsnoc xs of
    Just (xs,Item x) -> Paren (gen_ "(") (xs `snoc` Item x{whitespace=""}) (gen_ ")"){whitespace=whitespace x}
    _ -> Paren (gen_ "(") xs (gen_ ")")

is x (Item y) = lexeme y == x
is x _ = False

getWhite (Item x) = whitespace x
getWhite (Paren _ _ x) = whitespace x

setWhite w (Item x) = Item x{whitespace=w}
setWhite w (Paren x y z) = Paren x y z{whitespace=w}

isCtor (Item x) = any isUpper $ take 1 $ lexeme x
isCtor _ = False

isField (Item x) = all (isLower ||^ (== '_')) $ take 1 $ lexeme x
isField _ = False

makeField :: [Paren Lexeme] -> String
makeField [x] = "@" ++ show (concatMap lexeme $ unparen x)
makeField xs = "@'(" ++ intercalate "," (map (show . concatMap lexeme . unparen) xs) ++ ")"


continue op (Paren a b c:xs) = Paren a (op b) c : op xs
continue op (x:xs) = x : op xs
continue op [] = []


---------------------------------------------------------------------
-- PREAMBLE

-- | Add the necessary extensions, imports and local definitions
editAddPreamble :: [Paren Lexeme] -> [Paren Lexeme]
editAddPreamble o@xs
    | (premodu, modu:modname@xs) <- break (is "module") xs
    , (prewhr, whr:xs) <- break (is "where") xs
    = gen prefix : nl : premodu ++ modu : prewhr ++ whr : nl : gen imports : nl : xs ++ [nl, gen $ trailing modname, nl]
    | otherwise = gen prefix : nl : gen imports : nl : xs ++ [nl, gen $ trailing [], nl]
    where
        prefix = "{-# LANGUAGE DuplicateRecordFields, DataKinds, FlexibleInstances, TypeApplications, FlexibleContexts, MultiParamTypeClasses, OverloadedLabels #-}"
        imports = "import qualified GHC.Records.Extra as Z"
        -- if you import two things that have preprocessor_unused, and export them as modules, you don't want them to clash
        trailing modName = "_preprocessor_unused_" ++ uniq ++ " :: Z.HasField \"\" r a => r -> a;" ++
                           "_preprocessor_unused_" ++ uniq ++ " = Z.getField @\"\""
            where uniq = map (\x -> if isAlphaNum x then x else '_') $ concat $ take 19 $ takeWhile modPart $ map lexeme $ unparens modName
        modPart x = x == "." || all isUpper (take 1 x)


---------------------------------------------------------------------
-- SELECTORS

-- | a.b.c ==> getField @'(b,c) a
editSelectors :: [Paren Lexeme] -> [Paren Lexeme]
editSelectors (x:dot:field:rest)
    | null $ getWhite x, null $ getWhite dot
    , is "." dot
    , isField field
    , not $ isCtor x
    = editSelectors $
        paren ([gen "Z.getField", spc, gen (makeField [field]), spc] ++ editSelectors [x]) :
        [setWhite (getWhite field) (gen "") | getWhite field /= ""] ++ rest
editSelectors xs = continue editSelectors xs


---------------------------------------------------------------------
-- UPDATES

type Field = Paren Lexeme -- passes isField, has had atField applied
data Update = Update
    (Paren Lexeme) -- The expression being updated
    [([Field], Maybe (Paren Lexeme), Paren Lexeme)] -- (fields, operator, body)

renderUpdate :: Update -> [Paren Lexeme]
renderUpdate (Update e upd) = case unsnoc upd of
    Nothing -> [e]
    Just (rest, (field, operator, body)) -> return $ paren $
        [gen $ if isNothing operator then "Z.setField" else "Z.modifyField"
        ,spc
        ,gen $ makeField field
        ,spc] ++
        renderUpdate (Update e rest) ++
        [paren $ [if is "-" o then gen "subtract" else o | Just o <- [operator]] ++ [spc, body]]

-- e.a{b.c=d, ...} ==> e . #a & #b . #c .~ d & ...
editUpdates :: [Paren Lexeme] -> [Paren Lexeme]
editUpdates (e:xs)
    | not $ isCtor e
    , (fields, xs) <- spanFields1 xs
    , Paren brace inner end:xs <- xs
    , lexeme brace == "{"
    , Just updates <- mapM f $ split (is ",") inner
    , let end2 = [Item end{lexeme=""} | whitespace end /= ""]
    = paren (renderUpdate (Update (paren $ editUpdates (e : fields)) updates)) : end2 ++ editUpdates xs
    where
        spanFields1 (x:y:xs)
            | null $ getWhite x, is "." x
            , isField y
            = first ([x,y] ++) $ spanFields1 xs
        spanFields1 xs = ([], xs)

        spanFields2 (x:y:xs)
            | null $ getWhite x, is "." x
            , isField y
            = first (y:) $ spanFields2 xs
        spanFields2 xs = ([], xs)

        f (field1:xs)
            | isField field1
            , (fields, xs) <- spanFields2 xs
            , op:xs <- xs
            = Just (field1:fields, if is "=" op then Nothing else Just op, paren xs)
        f xs = Nothing
editUpdates xs = continue editUpdates xs


---------------------------------------------------------------------
-- INSTANCES

editAddInstances :: [Paren Lexeme] -> [Paren Lexeme]
editAddInstances xs = xs ++ concatMap (\x -> [nl, gen x])
    [ "instance Z.HasField \"" ++ fname ++ "\" " ++ rtyp ++ " (" ++ ftyp ++ ") " ++
      "where hasField _r = (\\_x -> _r{" ++ fname ++ "=_x}, (" ++ fname ++ ":: " ++ rtyp ++ " -> " ++ ftyp ++ ") _r)"
    | Record rname rargs fields <- parseRecords xs
    , let rtyp = "(" ++ unwords (rname : rargs) ++ ")"
    , (fname, ftyp) <- fields
    ]

-- | Represent a record, ignoring constructors. For example:
--
-- > data Type a b = Ctor1 {field1 :: Int, field2 :: String} | Ctor2 {field1 :: Int, field3 :: [Bool]}
--
--   Gets parsed as:
--
-- > Record "Type"] ["a","b"] [("field1","Int"), ("field2","String"), ("field3","[Bool]")]
data Record = Record
    String -- Name of the type (not constructor)
    [String] -- Type arguments
    [(String, String)] -- (field, type) - nub'd
    deriving Show

-- | Find all the records and parse them
parseRecords :: [Paren Lexeme] -> [Record]
parseRecords = mapMaybe whole . drop 1 . split (isPL "data" ||^ isPL "newtype")
    where
        whole :: [Paren Lexeme] -> Maybe Record
        whole xs
            | PL typeName : xs <- xs
            , (typeArgs, _:xs) <- break (isPL "=") xs
            = Just $ Record typeName [x | PL x <- typeArgs] $ nubOrd $ ctor xs
        whole _ = Nothing

        ctor xs
            | PL ctorName : Paren (L "{") inner _ : xs <- xs
            = fields (map (break (isPL "::")) $ split (isPL ",") inner) ++
              case xs of
                PL "|":xs -> ctor xs
                _ -> []
        ctor _ = []

        fields ((x,[]):(y,z):rest) = fields $ (x++y,z):rest
        fields ((names, _:typ):rest) = [(name, dropWhile (== '!') $ trim $ unlexer $ unparens typ) | PL name <- names] ++ fields rest
        fields _ = []
