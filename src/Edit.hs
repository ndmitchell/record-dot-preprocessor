
module Edit(edit) where

import Lexer
import Paren
import Data.Maybe
import Data.Char
import Data.List.Extra


edit :: [Paren Lexeme] -> [Paren Lexeme]
edit = editAddPreamble . editAddInstances . editSelectors


nl = Item $ Lexeme 0 0 "\n" ""
spc = Item $ Lexeme 0 0 " " ""
gen = Item . gen_
gen_ = Lexeme 0 0 ""

-- Add the necessary extensions, imports and local definitions
editAddPreamble :: [Paren Lexeme] -> [Paren Lexeme]
editAddPreamble xs = concatMap (\x -> [gen x, nl]) (prefix ++ imports) ++ xs
    where
        prefix = ["{-# LANGUAGE DuplicateRecordFields, DataKinds, FlexibleInstances, MultiParamTypeClasses, GADTs, OverloadedLabels #-}"]
        imports = ["import qualified GHC.OverloadedLabels as Z"
                  ,"import qualified Control.Lens as Z"]


-- a.b.c ==> ((a ^. #b) ^. #c)
editSelectors :: [Paren Lexeme] -> [Paren Lexeme]
editSelectors (x:Item dot:Item field:rest)
    | whitespace dot == "", lexeme dot == "."
    , whitespace field == "", all isLower $ take 1 $ lexeme field
    = editSelectors $ Paren (gen_ "(") [x, spc, Item dot{lexeme="Z.^."}, spc, Item field{lexeme='#':lexeme field}] (gen_ ")") : rest
editSelectors (x:xs) = x : editSelectors xs
editSelectors [] = []


editAddInstances :: [Paren Lexeme] -> [Paren Lexeme]
editAddInstances xs = xs ++ concatMap (\x -> [nl, gen x])
    [ "instance (Functor f, r ~ " ++ ftyp ++ ") => Z.IsLabel \"" ++ fname ++ "\" " ++
      "((r -> f r) -> " ++ rtyp ++ " -> f " ++ rtyp ++ ") " ++
      "where fromLabel = Z.lens (\\x -> " ++ fname ++ " (x :: " ++ rtyp ++ ")) (\\c x -> c{" ++ fname ++ "=x} :: " ++ rtyp ++ ")"
    | Record rname rargs fields <- parseRecords $ map (fmap lexeme) xs
    , let rtyp = unwords $ rname : rargs
    , (fname, ftyp) <- fields
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
        fields ((names, _:typ):rest) = [(name, concat $ unparen typ) | Item name <- names] ++ fields rest
        fields _ = []
