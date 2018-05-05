
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


nl = Item $ Lexeme 0 0 "" "\n"
spc = Item $ Lexeme 0 0 "" " "
gen = Item . gen_
gen_ x = Lexeme 0 0 x ""

paren xs = case unsnoc xs of
    Just (xs,Item x) -> Paren (gen_ "(") (xs `snoc` Item x{whitespace=""}) (gen_ ")"){whitespace=whitespace x}
    _ -> Paren (gen_ "(") xs (gen_ ")")

is x (Item y) = lexeme y == x
is x _ = False

noWhitespace (Item x) = null $ whitespace x
noWhitespace (Paren _ _ x) = null $ whitespace x

isCtor (Item x) = any isUpper $ take 1 $ lexeme x
isCtor _ = False

isField (Item x) = all (isLower ||^ (== '_')) $ take 1 $ lexeme x
isField _ = False

hashField (Item x)
    | c1:cs <- lexeme x
    , c1 == '_' && all isDigit cs -- tuple projection
    = Item x{lexeme = "Z." ++ lexeme x}
hashField (Item x) = Item x{lexeme = '#' : lexeme x}
hashField x = x


-- Add the necessary extensions, imports and local definitions
editAddPreamble :: [Paren Lexeme] -> [Paren Lexeme]
editAddPreamble xs
    | (premodu, modu:xs) <- break (is "module") xs
    , (prewhr, whr:xs) <- break (is "where") xs
    = gen prefix : nl : premodu ++ modu : prewhr ++ whr : nl : gen imports : nl : xs
    | otherwise = gen prefix : nl : gen imports : nl : xs
    where
        prefix = "{-# LANGUAGE DuplicateRecordFields, DataKinds, FlexibleInstances, MultiParamTypeClasses, GADTs, OverloadedLabels #-}"
        imports = "import qualified GHC.OverloadedLabels as Z; import qualified Control.Lens as Z"


continue op (Paren a b c:xs) = Paren a (op b) c : op xs
continue op (x:xs) = x : op xs
continue op [] = []


-- a.b.c ==> ((a ^. #b) ^. #c)
editSelectors :: [Paren Lexeme] -> [Paren Lexeme]
editSelectors (x:dot:field:rest)
    | noWhitespace x, noWhitespace dot
    , is "." dot
    , isField field
    = editSelectors $ paren (editSelectors [x] ++ [spc, gen "Z.^.", spc, hashField field]) : rest
editSelectors xs = continue editSelectors xs


type Field = Paren Lexeme -- passes isField, has had hashField applied
data Update = Update (Paren Lexeme) [Field] [([Field], Maybe (Paren Lexeme), Paren Lexeme)]
    -- expression, fields, then (fields, operator, body)

renderUpdate :: Update -> [Paren Lexeme]
renderUpdate (Update e fields upd) =
    e : spc : gen "Z.&" : spc :
    concat [[x, spc, gen "Z.%~", spc] | x <- fields] ++
    paren (intercalate [spc, gen ".", spc] $ map (pure . paren)
        [ concat [ [x, spc, gen "Z.%~", spc] | x <- fields] ++ [paren [fromMaybe (gen "const") op, body]]
        | (fields, op, body) <- reverse upd]
    ) : []


-- e.a{b.c=d, ...} ==> e . #a & #b . #c .~ d & ...
editUpdates :: [Paren Lexeme] -> [Paren Lexeme]
editUpdates (e:xs)
    | noWhitespace e, not $ isCtor e
    , (fields, xs) <- spanFields xs
    , Paren brace inner end:xs <- xs
    , lexeme brace == "{"
    , Just updates <- mapM f $ split (is ",") inner
    , let end2 = [Item end{lexeme=""} | whitespace end /= ""]
    = paren (renderUpdate (Update e fields updates)) : end2 ++ editUpdates xs
    where
        spanFields (x:y:xs)
            | noWhitespace x, is "." x
            , isField y
            = first (hashField y:) $ spanFields xs
        spanFields xs = ([], xs)

        f (field1:xs)
            | isField field1
            , (fields, xs) <- spanFields xs
            , op:xs <- xs
            = Just (hashField field1:fields, if is "=" op then Nothing else Just op, paren xs)
        f xs = Nothing
editUpdates xs = continue editUpdates xs


editAddInstances :: [Paren Lexeme] -> [Paren Lexeme]
editAddInstances xs = xs ++ concatMap (\x -> [nl, gen x])
    [ "instance (" ++ intercalate ", " context ++ ") => Z.IsLabel \"" ++ fname ++ "\" " ++
      "((t1 -> f t2) -> " ++ rtyp ++ " -> f t3) " ++
      "where fromLabel = Z.lens (\\x -> " ++ fname ++ " (x :: " ++ rtyp ++ ")) (\\c x -> c{" ++ fname ++ "=x} :: " ++ rtyp ++ ")"
    | Record rname rargs fields <- parseRecords $ map (fmap lexeme) xs
    , let rtyp = "(" ++ unwords (rname : rargs) ++ ")"
    , (fname, ftyp) <- fields
    , let context = ["Functor f", "t1 ~ " ++ ftyp, "t2 ~ t1", "t3 ~ " ++ rtyp]
    ]



data Record = Record String [String] [(String, String)] -- TypeName TypeArgs [(FieldName, FieldType)]
    deriving Show

parseRecords :: [Paren String] -> [Record]
parseRecords = mapMaybe whole . drop 1 . split (`elem` [Item "data", Item "newtype"])
    where
        whole :: [Paren String] -> Maybe Record
        whole xs
            | Item typeName : xs <- xs
            , (typeArgs, _:xs) <- break (== Item "=") xs
            = Just $ Record typeName [x | Item x <- typeArgs] $ nubOrd $ ctor xs
        whole _ = Nothing

        ctor xs
            | Item ctorName : Paren "{" inner _ : xs <- xs
            = fields (map (break (== Item "::")) $ split (== Item ",") inner) ++
              maybe [] ctor (stripPrefix [Item "|"] xs)
        ctor _ = []

        fields ((x,[]):(y,z):rest) = fields $ (x++y,z):rest
        fields ((names, _:typ):rest) = [(name, unwords $ unparen typ) | Item name <- names] ++ fields rest
        fields _ = []
