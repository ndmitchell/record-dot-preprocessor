{-# LANGUAGE RecordWildCards, ViewPatterns, NamedFieldPuns #-}
{- HLINT ignore "Use camelCase" -}

-- | Module containing the plugin.
module RecordDotPreprocessor(plugin) where

import Data.Generics.Uniplate.Data
import Data.List.Extra
import Data.Tuple.Extra
import Compat
import Bag
import qualified GHC
import qualified GhcPlugins as GHC
import HsSyn
import SrcLoc
import TcEvidence


---------------------------------------------------------------------
-- PLUGIN WRAPPER

-- | GHC plugin.
plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.parsedResultAction = \_cliOptions _modSummary x -> return x{GHC.hpm_module = onModule <$> GHC.hpm_module x}
    , GHC.pluginRecompile = GHC.purePlugin
    }


---------------------------------------------------------------------
-- PLUGIN GUTS

setL :: SrcSpan -> GenLocated SrcSpan e -> GenLocated SrcSpan e
setL l (L _ x) = L l x

mod_records :: GHC.ModuleName
mod_records = GHC.mkModuleName "GHC.Records.Extra"

var_HasField, var_hasField, var_getField, var_setField, var_dot :: GHC.RdrName
var_HasField = GHC.mkRdrQual mod_records $ GHC.mkClsOcc "HasField"
var_hasField = GHC.mkRdrUnqual $ GHC.mkVarOcc "hasField"
var_getField = GHC.mkRdrQual mod_records $ GHC.mkVarOcc "getField"
var_setField = GHC.mkRdrQual mod_records $ GHC.mkVarOcc "setField"
var_dot = GHC.mkRdrUnqual $ GHC.mkVarOcc "."


onModule :: HsModule GhcPs -> HsModule GhcPs
onModule x = x { hsmodImports = onImports $ hsmodImports x
               , hsmodDecls = concatMap onDecl $ hsmodDecls x
               }


onImports :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
onImports = (:) $ noL $ GHC.ImportDecl GHC.NoExt GHC.NoSourceText (noL mod_records)
    Nothing False False True {- qualified -} True {- implicit -} Nothing Nothing


{-
instance Z.HasField "name" (Company) (String) where hasField _r = (\_x -> _r{name=_x}, (name:: (Company) -> String) _r)

instance HasField "selector" Record Field where
    hasField r = (\x -> r{selector=x}, (name :: Record -> Field) r)
-}
instanceTemplate :: FieldOcc GhcPs -> HsType GhcPs -> HsType GhcPs -> InstDecl GhcPs
instanceTemplate selector record field = ClsInstD noE $ ClsInstDecl noE (HsIB noE typ) (unitBag has) [] [] [] Nothing
    where
        typ = mkHsAppTys
            (noL (HsTyVar noE GHC.NotPromoted (noL var_HasField)))
            [noL (HsTyLit noE (HsStrTy GHC.NoSourceText (GHC.occNameFS $ GHC.occName $ unLoc $ rdrNameFieldOcc selector)))
            ,noL record
            ,noL field
            ]

        has :: LHsBindLR GhcPs GhcPs
        has = noL $ FunBind noE (noL var_hasField) (mg1 eqn) WpHole []
            where
                eqn = Match
                    { m_ext     = noE
                    , m_ctxt    = FunRhs (noL var_hasField) GHC.Prefix NoSrcStrict
                    , m_pats    = compat_m_pats [VarPat noE $ noL vR]
                    , m_grhss   = GRHSs noE [noL $ GRHS noE [] $ noL $ ExplicitTuple noE [noL $ Present noE set, noL $ Present noE get] GHC.Boxed] (noL $ EmptyLocalBinds noE)
                    }
                set = noL $ HsLam noE $ mg1 Match
                    { m_ext     = noE
                    , m_ctxt    = LambdaExpr
                    , m_pats    = compat_m_pats [VarPat noE $ noL vX]
                    , m_grhss   = GRHSs noE [noL $ GRHS noE [] $ noL update] (noL $ EmptyLocalBinds noE)
                    }
                update = RecordUpd noE (noL $ GHC.HsVar noE $ noL vR)
                    [noL $ HsRecField (noL (Unambiguous noE (rdrNameFieldOcc selector))) (noL $ GHC.HsVar noE $ noL vX) False]
                get = mkApp
                    (mkParen $ mkTypeAnn (noL $ GHC.HsVar noE $ rdrNameFieldOcc selector) (noL $ HsFunTy noE (noL record) (noL field)))
                    (noL $ GHC.HsVar noE $ noL vR)

        mg1 :: Match GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
        mg1 x = MG noE (noL [noL x]) GHC.Generated

        vR = GHC.mkRdrUnqual $ GHC.mkVarOcc "r"
        vX = GHC.mkRdrUnqual $ GHC.mkVarOcc "x"


onDecl :: LHsDecl GhcPs -> [LHsDecl GhcPs]
onDecl o@(L _ (GHC.TyClD _ x)) = o :
    [ noL $ InstD noE $ instanceTemplate field (unLoc record) (unbang typ)
    | let fields = nubOrdOn (\(_,_,x,_) -> GHC.occNameFS $ GHC.rdrNameOcc $ unLoc $ rdrNameFieldOcc x) $ getFields x
    , (record, _, field, typ) <- fields]
onDecl x = [descendBi onExp x]

unbang :: HsType GhcPs -> HsType GhcPs
unbang (HsBangTy _ _ x) = unLoc x
unbang x = x

getFields :: TyClDecl GhcPs -> [(LHsType GhcPs, IdP GhcPs, FieldOcc GhcPs, HsType GhcPs)]
getFields DataDecl{tcdDataDefn=HsDataDefn{..}, ..} = concatMap ctor dd_cons
    where
        ctor (L _ ConDeclH98{con_args=RecCon (L _ fields),con_name=L _ name}) = concatMap (field name) fields
        ctor _ = []

        field name (L _ ConDeclField{cd_fld_type=L _ ty, ..}) = [(result, name, fld, ty) | L _ fld <- cd_fld_names]
        field _ _ = error "unknown field declaration in getFields"

        result = noL $ HsParTy noE $ foldl (\x y -> noL $ HsAppTy noE x $ hsLTyVarBndrToType y) (noL $ HsTyVar noE GHC.NotPromoted tcdLName) $ hsq_explicit tcdTyVars
getFields _ = []


-- At this point infix expressions have not had associativity/fixity applied, so they are bracketed
-- a + b + c ==> (a + b) + c
-- Therefore we need to deal with, in general:
-- x.y, where
-- x := a | a b | a.b | a + b
-- y := a | a b | a{b=1}
onExp :: LHsExpr GhcPs -> LHsExpr GhcPs
onExp (L o (OpApp _ lhs mid@(isDot -> True) rhs))
    | adjacent lhs mid, adjacent mid rhs
    , (lhsOp, lhs) <- getOpRHS $ onExp lhs
    , (lhsApp, lhs) <- getAppRHS lhs
    , (rhsApp, rhs) <- getAppLHS rhs
    , (rhsRec, rhs) <- getRec rhs
    , Just sel <- getSelector rhs
    = onExp $ lhsOp $ rhsApp $ lhsApp $ rhsRec $ mkParen $ setL o $ mkVar var_getField `mkAppType` sel `mkApp` lhs

-- Turn (.foo.bar) into getField calls
onExp (L o (SectionR _ mid@(isDot -> True) rhs))
    | adjacent mid rhs
    , srcSpanStart o == srcSpanStart (getLoc mid)
    , srcSpanEnd o == srcSpanEnd (getLoc rhs)
    , Just sels <- getSelectors rhs
    = setL o $ mkParen $ foldl1 (\x y -> noL $ OpApp noE x (mkVar var_dot) y) $ map (mkVar var_getField `mkAppType`) $ reverse sels

-- Turn a{b=c, d=e, ...} into successive setField calls
onExp (L _ RecordUpd{rupd_expr,rupd_flds=[]}) = onExp rupd_expr
onExp (L o upd@RecordUpd{rupd_expr,rupd_flds=L _ (HsRecField (fmap rdrNameAmbiguousFieldOcc -> lbl) arg pun):flds})
    | let sel = mkSelector lbl
    , let arg2 = if pun then noL $ HsVar noE lbl else arg
    , let expr = mkParen $ mkVar var_setField `mkAppType` sel `mkApp` mkParen rupd_expr `mkApp` arg2
    = onExp $ L o upd{rupd_expr=expr,rupd_flds=flds}

onExp x = descend onExp x


mkSelector :: Located GHC.RdrName -> LHsType GhcPs
mkSelector (L o x) = L o $ HsTyLit noE $ HsStrTy GHC.NoSourceText $ GHC.occNameFS $ GHC.rdrNameOcc x

getSelector :: LHsExpr GhcPs -> Maybe (LHsType GhcPs)
getSelector (L _ (HsVar _ (L o sym)))
    | not $ GHC.isQual sym
    = Just $ mkSelector $ L o sym
getSelector _ = Nothing

-- | Turn a.b.c into Just [a,b,c]
getSelectors :: LHsExpr GhcPs -> Maybe [LHsType GhcPs]
getSelectors (L _ (OpApp _ lhs mid@(isDot -> True) rhs))
    | adjacent lhs mid, adjacent mid rhs
    , Just post <- getSelector rhs
    , Just pre <- getSelectors lhs
    = Just $ pre ++ [post]
getSelectors x = (:[]) <$> getSelector x

-- | Lens on: f [x]
getAppRHS :: LHsExpr GhcPs -> (LHsExpr GhcPs -> LHsExpr GhcPs, LHsExpr GhcPs)
getAppRHS (L l (HsApp e x y)) = (L l . HsApp e x, y)
getAppRHS x = (id, x)

-- | Lens on: [f] x y z
getAppLHS :: LHsExpr GhcPs -> (LHsExpr GhcPs -> LHsExpr GhcPs, LHsExpr GhcPs)
getAppLHS (L l (HsApp e x y)) = first (\c -> L l . (\x -> HsApp e x y) . c) $ getAppLHS x
getAppLHS x = (id, x)

-- | Lens on: a + [b]
getOpRHS :: LHsExpr GhcPs -> (LHsExpr GhcPs -> LHsExpr GhcPs, LHsExpr GhcPs)
getOpRHS (L l (OpApp x y p z)) = (L l . OpApp x y p, z)
getOpRHS x = (id, x)

-- | Lens on: [r]{f1=x1}{f2=x2}
getRec :: LHsExpr GhcPs -> (LHsExpr GhcPs -> LHsExpr GhcPs, LHsExpr GhcPs)
getRec (L l r@RecordUpd{}) = first (\c x -> L l r{rupd_expr=c x}) $ getRec $ rupd_expr r
getRec x = (id, x)

-- | Is it equal to: .
isDot :: LHsExpr GhcPs -> Bool
isDot (L _ (HsVar _ (L _ op))) = op == var_dot
isDot _ = False

mkVar :: GHC.RdrName -> LHsExpr GhcPs
mkVar = noL . HsVar noE . noL

mkParen :: LHsExpr GhcPs -> LHsExpr GhcPs
mkParen = noL . HsPar noE

mkApp :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
mkApp x y = noL $ HsApp noE x y

-- | Are the end of a and the start of b next to each other, no white space
adjacent :: Located a -> Located b -> Bool
adjacent (L a _) (L b _) = isGoodSrcSpan a && srcSpanEnd a == srcSpanStart b
