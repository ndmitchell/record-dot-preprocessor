{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Edit(edit) where

import Lexer
import Paren
import Data.Maybe
import Data.Char
import Data.List.Extra
import Data.Tuple.Extra
import Control.Monad.Extra
import Debug.Trace


edit :: [PL] -> [PL]
edit = editAddPreamble . editAddInstances . editLoop


---------------------------------------------------------------------
-- HELPERS

-- Projecting in on the 'lexeme' inside
type L = Lexeme
unL = lexeme
mkL x = Lexeme 0 0 x ""
pattern L x <- (unL -> x)

-- Projecting in on the lexeme inside an Item
type PL = Paren L
unPL (Item (L x)) = Just x
unPL _ = Nothing
isPL x y = unPL y == Just x
pattern PL x <- (unPL -> Just x)
mkPL = Item . mkL

-- Whitespace
pattern NoW x <- ((\v -> if null $ getWhite v then Just v else Nothing) -> Just x)




nl = Item $ Lexeme 0 0 "" "\n"
spc = Item $ Lexeme 0 0 "" " "


paren [x] = x
paren xs = case unsnoc xs of
    Just (xs,Item x) -> Paren (mkL "(") (xs `snoc` Item x{whitespace=""}) (mkL ")"){whitespace=whitespace x}
    _ -> Paren (mkL "(") xs (mkL ")")


getWhite (Item x) = whitespace x
getWhite (Paren _ _ x) = whitespace x

setWhite w (Item x) = Item x{whitespace=w}
setWhite w (Paren x y z) = Paren x y z{whitespace=w}

isCtor (Item x) = any isUpper $ take 1 $ lexeme x
isCtor _ = False

isField (Item x) = all (isLower ||^ (== '_')) $ take 1 $ lexeme x
isField _ = False

makeField :: [String] -> String
makeField [x] = "@" ++ show x
makeField xs = "@'(" ++ intercalate "," (map show xs) ++ ")"

makeFieldOld :: [PL] -> String
makeFieldOld [x] = "@" ++ show (concatMap lexeme $ unparen x)
makeFieldOld xs = "@'(" ++ intercalate "," (map (show . concatMap lexeme . unparen) xs) ++ ")"


---------------------------------------------------------------------
-- PREAMBLE

-- | Add the necessary extensions, imports and local definitions
editAddPreamble :: [PL] -> [PL]
editAddPreamble o@xs
    | (premodu, modu:modname@xs) <- break (isPL "module") xs
    , (prewhr, whr:xs) <- break (isPL "where") xs
    = mkPL prefix : nl : premodu ++ modu : prewhr ++ whr : nl : mkPL imports : nl : xs ++ [nl, mkPL $ trailing modname, nl]
    | otherwise = mkPL prefix : nl : mkPL imports : nl : xs ++ [nl, mkPL $ trailing [], nl]
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

-- given e.lbl1.lbl2 return (e, [lbl1,lbl2], whitespace, rest)
spanFields :: [PL] -> Maybe (PL, [String], String, [PL])
spanFields (NoW e:xs) = let (a,b,c) = f xs in if null a then Nothing else Just (e,a,b,c)
    where
        f (NoW (PL "."):x@(PL (fld@(fld1:_))):xs) | fld1 == '_' || isLower fld1 = (\(a,b,c) -> (fld:a,b,c)) $
            case x of NoW{} -> f xs; _ -> ([], getWhite x, xs)
        f xs = ([], "", xs)
spanFields _ = Nothing


editLoop :: [PL] -> [PL]

-- | a.b.c ==> getField @'(b,c) a
editLoop (spanFields -> Just (e, fields, whitespace, rest))
    | not $ isCtor e
    = editLoop $ setWhite whitespace (paren ([mkPL "Z.getField", spc, mkPL (makeField fields), spc] ++ [e])) : rest

-- e.a{b.c=d, ...} ==> e . #a & #b . #c .~ d & ...
editLoop (e:Paren (L "{") inner end:xs)
    | not $ isCtor e
    , Just updates <- mapM f $ split (isPL ",") inner
    , let end2 = [Item end{lexeme=""} | whitespace end /= ""]
    = editLoop $ paren (renderUpdate (Update e updates)) : end2 ++ xs
    where
        spanFields2 (x:y:xs)
            | null $ getWhite x, isPL "." x
            , isField y
            = first (y:) $ spanFields2 xs
        spanFields2 xs = ([], xs)

        f (field1:xs)
            | isField field1
            , (fields, xs) <- spanFields2 xs
            , op:xs <- xs
            = Just (field1:fields, if isPL "=" op then Nothing else Just op, paren xs)
        f xs = Nothing

editLoop (Paren a b c:xs) = Paren a (editLoop b) c : editLoop xs
editLoop (x:xs) = x : editLoop xs
editLoop [] = []


---------------------------------------------------------------------
-- UPDATES

type Field = PL -- passes isField, has had atField applied
data Update = Update
    (PL) -- The expression being updated
    [([Field], Maybe (PL), PL)] -- (fields, operator, body)

renderUpdate :: Update -> [PL]
renderUpdate (Update e upd) = case unsnoc upd of
    Nothing -> [e]
    Just (rest, (field, operator, body)) -> return $ paren $
        [mkPL $ if isNothing operator then "Z.setField" else "Z.modifyField"
        ,spc
        ,mkPL $ makeFieldOld field
        ,spc] ++
        renderUpdate (Update e rest) ++
        [paren $ [if isPL "-" o then mkPL "subtract" else o | Just o <- [operator]] ++ [spc, body]]


---------------------------------------------------------------------
-- INSTANCES

editAddInstances :: [PL] -> [PL]
editAddInstances xs = xs ++ concatMap (\x -> [nl, mkPL x])
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
parseRecords :: [PL] -> [Record]
parseRecords = mapMaybe whole . drop 1 . split (isPL "data" ||^ isPL "newtype")
    where
        whole :: [PL] -> Maybe Record
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
