{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Edit(edit) where

import Lexer
import Paren
import Data.Maybe
import Data.Char
import Data.List.Extra
import Control.Monad.Extra


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
pattern NoW x <- (\v -> if null $ getWhite v then Just v else Nothing -> Just x)


paren [x] = x
paren xs = case unsnoc xs of
    Just (xs,x) -> Paren (mkL "(") (xs `snoc` setWhite "" x) (mkL ")"){whitespace = getWhite x}
    _ -> Paren (mkL "(") xs (mkL ")")

spc = addWhite " "
nl = addWhite "\n"

addWhite w x = setWhite (getWhite x ++ w) x

getWhite (Item x) = whitespace x
getWhite (Paren _ _ x) = whitespace x

setWhite w (Item x) = Item x{whitespace=w}
setWhite w (Paren x y z) = Paren x y z{whitespace=w}

isCtor (Item x) = any isUpper $ take 1 $ lexeme x
isCtor _ = False

-- | This test does not check that the @quoter@ name is a qualified identifier,
-- instead relying on lack of whitespace in the opener and existence of a paired
-- closed (@|]@)
isQuasiQuotation :: PL -> Bool
isQuasiQuotation (Paren (L "[") inner@(Item qq : Item (L "|") : _) (L "]"))
    | Item close@(L "|") <- last inner = null (whitespace qq) && null (whitespace close)
isQuasiQuotation _ = False

isField (x:_) = x == '_' || isLower x
isField _ = False

makeField :: [String] -> String
makeField [x] = "@" ++ show x
makeField xs = "@'(" ++ intercalate "," (map show xs) ++ ")"


---------------------------------------------------------------------
-- PREAMBLE

-- | Add the necessary extensions, imports and local definitions
editAddPreamble :: [PL] -> [PL]
editAddPreamble o@xs
    | (premodu, modu:modname@xs) <- break (isPL "module") xs
    , (prewhr, whr:xs) <- break (isPL "where") xs
    = nl (mkPL prefix) : premodu ++ modu : prewhr ++ whr : nl (mkPL "") : nl (mkPL imports) : xs ++ [nl $ mkPL "", nl $ mkPL $ trailing modname]
    | otherwise = blanks ++ nl (mkPL prefix) : nl (mkPL imports) : rest ++ [nl $ mkPL "", nl $ mkPL $ trailing []]
    where
        (blanks, rest) = span (isPL "") o

        prefix = "{-# LANGUAGE DuplicateRecordFields, DataKinds, FlexibleInstances, TypeApplications, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeOperators, GADTs, UndecidableInstances #-}\n" ++
                 -- it's too hard to avoid generating excessive brackets, so just ignore the code
                 -- only really applies to people using it through Haskell Language Server (see #37)
                 "{- HLINT ignore \"Redundant bracket\" -}"
        imports = "import qualified GHC.Records.Extra as Z"
        -- if you import two things that have preprocessor_unused, and export them as modules, you don't want them to clash
        trailing modName = "_recordDotPreprocessorUnused" ++ uniq ++ " :: Z.HasField \"\" r a => r -> a;" ++
                           "_recordDotPreprocessorUnused" ++ uniq ++ " = Z.getField @\"\""
            where uniq = filter isAlphaNum $ concat $ take 19 $ takeWhile modPart $ map lexeme $ unparens modName
        modPart x = x == "." || all isUpper (take 1 x)


---------------------------------------------------------------------
-- SELECTORS

-- given .lbl1.lbl2 return ([lbl1,lbl2], whitespace, rest)
spanFields :: [PL] -> ([String], String, [PL])
spanFields (NoW (PL "."):x@(PL fld):xs) | isField fld = (\(a,b,c) -> (fld:a,b,c)) $
    case x of NoW{} -> spanFields xs; _ -> ([], getWhite x, xs)
spanFields xs = ([], "", xs)


editLoop :: [PL] -> [PL]

--  Leave quasiquotations alone
editLoop (p : ps) | isQuasiQuotation p = p : editLoop ps

-- | a.b.c ==> getField @'(b,c) a
editLoop (NoW e : (spanFields -> (fields@(_:_), whitespace, rest)))
    | not $ isCtor e
    = editLoop $ addWhite whitespace (paren [spc $ mkPL "Z.getField", spc $ mkPL $ makeField fields, e]) : rest

-- (.a.b) ==> (getField @'(a,b))
editLoop (Paren start@(L "(") (spanFields -> (fields@(_:_), whitespace, [])) end:xs)
    = editLoop $ Paren start [spc $ mkPL "Z.getField", addWhite whitespace $ mkPL $ makeField fields] end : xs

-- e{b.c=d, ...} ==> setField @'(b,c) d
editLoop (e:Paren (L "{") inner end:xs)
    | not $ isCtor e
    , not $ isPL "::" e
    , getWhite e == ""
    , Just updates <- mapM f $ split (isPL ",") inner
    , let end2 = [Item end{lexeme=""} | whitespace end /= ""]
    = editLoop $ renderUpdate (Update e updates) : end2 ++ xs
    where
        f (NoW (PL field1) : (spanFields -> (fields, whitespace, xs)))
            | isField field1
            = g (field1:fields) xs
        f (x@(PL field1):xs)
            | isField field1
            = g [field1] xs
        f _ = Nothing

        g fields (op:xs) = Just (fields, if isPL "=" op then Nothing else Just op, Just $ paren xs)
        g fields [] = Just (fields, Nothing, Nothing)


editLoop (Paren a b c:xs) = Paren a (editLoop b) c : editLoop xs
editLoop (x:xs) = x : editLoop xs
editLoop [] = []


---------------------------------------------------------------------
-- UPDATES

data Update = Update
    PL -- The expression being updated
    [([String], Maybe PL, Maybe PL)] -- (fields, operator, body)

renderUpdate :: Update -> PL
renderUpdate (Update e upd) = case unsnoc upd of
    Nothing -> e
    Just (rest, (field, operator, body)) -> paren
        [spc $ mkPL $ if isNothing operator then "Z.setField" else "Z.modifyField"
        ,spc $ mkPL $ makeField $ if isNothing body then [last field] else field
        ,spc (renderUpdate (Update e rest))
        ,case (operator, body) of
            (Just o, Just b) -> paren [spc $ if isPL "-" o then mkPL "subtract" else o, b]
            (Nothing, Just b) -> b
            (Nothing, Nothing)
                | [field] <- field -> mkPL field
                | f1:fs <- field -> paren [spc $ mkPL "Z.getField", spc $ mkPL $ makeField fs, mkPL f1]
            _ -> error "renderUpdate, internal error"
        ]


---------------------------------------------------------------------
-- INSTANCES

editAddInstances :: [PL] -> [PL]
editAddInstances xs = xs ++ concatMap (\x -> [nl $ mkPL "", mkPL x])
    [ "instance (aplg ~ (" ++ ftyp ++ ")) => Z.HasField \"" ++ fname ++ "\" " ++ rtyp ++ " aplg " ++
      "where hasField _r = (\\_x -> case _r of {" ++ intercalate " ; "
        [ if fname `elem` map fst fields then
            "(" ++ cname ++ " " ++
                unwords [if fst field == fname then "_" else "_x" ++ show i | (i, field) <- zipFrom 1 fields] ++
            ") -> " ++ cname ++ " " ++
                unwords [if fst field == fname then "_x" else "_x" ++ show i | (i, field) <- zipFrom 1 fields]
          else
            cname ++ "{} -> Prelude.error " ++ show ("Cannot update " ++ msg cname)
        | Ctor cname fields <- ctors] ++
        "}, case _r of {" ++ intercalate " ; "
        [ if fname `elem` map fst fields then
            "(" ++ cname ++ " " ++
                unwords [if fst field == fname then "_x1" else "_" | field <- fields] ++
            ") -> _x1"
          else
            cname ++ "{} -> Prelude.error " ++ show ("Cannot get " ++ msg cname)
        | Ctor cname fields <- ctors] ++
      "})"
    | Record rname rargs ctors <- parseRecords xs
    , let rtyp = "(" ++ unwords (rname : rargs) ++ ")"
    , (fname, ftyp) <- nubOrd $ concatMap ctorFields ctors
    , let msg cname = "field " ++ show fname ++ " of type " ++ show rname ++ " with constructor " ++ show cname
    ]

-- | Represent a record, ignoring constructors. For example:
--
-- > data Type a b = Ctor1 {field1 :: Int, field2 :: String} | Ctor2 {field1 :: Int, field3 :: [Bool]}
--
--   Gets parsed as:
--
-- > Record "Type" ["a","b"]
-- >   [Ctor "Ctor1" [("field1","Int"), ("field2","String")]
-- >   [Ctor "Ctor2" [("field1","Int"), ("field3","[Bool]")]
data Record = Record
    {recordName :: String -- Name of the type (not constructor)
    ,recordTyArgs :: [String] -- Type arguments
    ,recordCtors :: [Ctor]
    }
    deriving Show

data Ctor = Ctor
    {ctorName :: String -- Name of constructor
    ,ctorFields :: [(String, String)] -- (field, type)
    }
    deriving Show



-- | Find all the records and parse them
parseRecords :: [PL] -> [Record]
parseRecords = mapMaybe whole . drop1 . split (isPL "data" ||^ isPL "newtype")
    where
        whole :: [PL] -> Maybe Record
        whole xs
            | PL typeName : xs <- xs
            , (typeArgs, _:xs) <- break (isPL "=" ||^ isPL "where") xs
            = Just $ Record typeName (mapMaybe typeArg typeArgs) $ ctor xs
        whole _ = Nothing

        -- some types are raw, some are in brackets (with a kind signature)
        typeArg (PL x) = Just x
        typeArg (Paren _ (x:_) _) = typeArg x
        typeArg _ = Nothing

        ctor xs
            | xs <- dropContext xs
            , PL ctorName : xs <- xs
            , xs <- dropWhile (isPL "::") xs
            , xs <- dropContext xs
            , Paren (L "{") inner _ : xs <- xs
            = Ctor ctorName (fields $ map (break (isPL "::")) $ split (isPL ",") inner) :
              case xs of
                PL "|":xs -> ctor xs
                _ -> []
        ctor _ = []

        -- we don't use a full parser so dealing with context like
        --   Num a => V3 { xx, yy, zz :: a }
        -- is hard. Fake it as best we can
        dropContext (Paren (L "(") _ _ : PL "=>" : xs) = xs
        dropContext (_ : _  : PL "=>": xs) = xs
        dropContext xs = xs

        fields ((x,[]):(y,z):rest) = fields $ (x++y,z):rest
        fields ((names, _:typ):rest) = [(name, dropWhile (== '!') $ trim $ unlexer $ unparens typ) | PL name <- names] ++ fields rest
        fields _ = []

        -- if the user has a trailing comment want to rip it out so our brackets still work
        unlexer = concatMap $ \x -> lexeme x ++ [' ' | whitespace x /= ""]
