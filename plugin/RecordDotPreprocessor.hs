{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards, ViewPatterns, NamedFieldPuns, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
{- HLINT ignore "Use camelCase" -}

-- | Module containing the plugin.
module RecordDotPreprocessor(plugin) where

import Data.Generics.Uniplate.Data
import Data.List.Extra
import Data.Tuple.Extra
import Compat
import qualified GHC
#if __GLASGOW_HASKELL__ < 900
import Bag
import qualified GhcPlugins as GHC
import qualified HscMain
import qualified PrelNames as GHC
import SrcLoc
#else
import GHC.Data.Bag
import qualified GHC.Driver.Plugins as GHC
import qualified GHC.Driver.Types as GHC
import qualified GHC.Builtin.Names as GHC
import qualified GHC.Plugins as GHC
import GHC.Types.SrcLoc
#endif

---------------------------------------------------------------------
-- PLUGIN WRAPPER

-- | GHC plugin.
plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.parsedResultAction = parsedResultAction
    , GHC.pluginRecompile = GHC.purePlugin
    }
    where
        parsedResultAction _cliOptions _modSummary x = do
            hscenv <- dropRnTraceFlags <$> HscMain.getHscEnv
            uniqSupply <- GHC.liftIO (GHC.mkSplitUniqSupply '0')
            uniqSupplyRef <- GHC.liftIO $ newIORef uniqSupply
            let ?hscenv = hscenv
            let ?uniqSupply = uniqSupplyRef
            pure x{GHC.hpm_module = onModule <$> GHC.hpm_module x}


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


onModule :: PluginEnv => Module -> Module
onModule x = x { hsmodImports = onImports $ hsmodImports x
               , hsmodDecls = concatMap (onDecl (unLoc <$> hsmodName x)) $ hsmodDecls x
               }


onImports :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
onImports = (:) $ qualifiedImplicitImport mod_records

{-
instance Z.HasField "name" (Company) (String) where hasField _r = (\_x -> _r{name=_x}, (name:: (Company) -> String) _r)

instance HasField "selector" Record Field where
    hasField r = (\x -> r{selector=x}, (name :: Record -> Field) r)
-}
instanceTemplate :: FieldOcc GhcPs -> HsType GhcPs -> HsType GhcPs -> InstDecl GhcPs
instanceTemplate selector record field = ClsInstD noE $ ClsInstDecl noE (HsIB noE typ) (unitBag has) [] [] [] Nothing
    where
        typ' a = mkHsAppTys
            (noL (HsTyVar noE GHC.NotPromoted (noL var_HasField)))
            [noL (HsTyLit noE (HsStrTy GHC.NoSourceText (GHC.occNameFS $ GHC.occName $ unLoc $ rdrNameFieldOcc selector)))
            ,noL record
            ,noL a
            ]

        typ = noL $ makeEqQualTy field (unLoc . typ')

        has :: LHsBindLR GhcPs GhcPs
        has = noL $ newFunBind (noL var_hasField) (mg1 eqn)
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
                    (mkParen $ mkTypeAnn (noL $ GHC.HsVar noE $ rdrNameFieldOcc selector) (mkFunTy (noL record) (noL field)))
                    (noL $ GHC.HsVar noE $ noL vR)

        mg1 :: Match GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
        mg1 x = MG noE (noL [noL x]) GHC.Generated

        vR = GHC.mkRdrUnqual $ GHC.mkVarOcc "r"
        vX = GHC.mkRdrUnqual $ GHC.mkVarOcc "x"


onDecl :: PluginEnv => Maybe GHC.ModuleName -> LHsDecl GhcPs -> [LHsDecl GhcPs]
onDecl modName o@(L _ (GHC.TyClD _ x)) = o :
    [ noL $ InstD noE $ instanceTemplate field (unLoc record) (unbang typ)
    | let fields = nubOrdOn (\(_,_,x,_) -> GHC.occNameFS $ GHC.rdrNameOcc $ unLoc $ rdrNameFieldOcc x) $ getFields modName x
    , (record, _, field, typ) <- fields]
onDecl _ x = [descendBi onExp x]

unbang :: HsType GhcPs -> HsType GhcPs
unbang (HsBangTy _ _ x) = unLoc x
unbang x = x

getFields :: PluginEnv => Maybe GHC.ModuleName -> TyClDecl GhcPs -> [(LHsType GhcPs, IdP GhcPs, FieldOcc GhcPs, HsType GhcPs)]
getFields modName DataDecl{tcdDataDefn=HsDataDefn{..}, ..} = concatMap ctor dd_cons
    where
        ctor (L _ con) = [(result, name, fld, ty) | (name, fld, ty) <- conClosedFields (defVars tcdTyVars) con]

        defVars :: LHsQTyVars GhcPs -> [GHC.RdrName]
        defVars vars = [v | L _ v <- hsLTyVarLocNames vars]

        -- A value of this data declaration will have this type.
        result = foldl (\x y -> noL $ HsAppTy noE x $ hsLTyVarBndrToType y) (noL $ HsTyVar noE GHC.NotPromoted tyName) $ hsq_explicit tcdTyVars
        tyName = case (tcdLName, modName) of
            (L l (GHC.Unqual name), Just modName') -> L l (GHC.Qual modName' name)
            _ -> tcdLName
getFields _ _ = []

-- Extract filed and its type from declaration, omitting fields with existential/higher-kind types.
conClosedFields :: PluginEnv => [GHC.RdrName] -> ConDecl GhcPs -> [(IdP GhcPs, FieldOcc GhcPs, HsType GhcPs)]
conClosedFields resultVars = \case
    ConDeclH98 {con_args = RecCon (L _ args), con_name, con_ex_tvs} ->
        [ (unLoc con_name, unLoc name, unLoc ty)
            | ConDeclField {cd_fld_names, cd_fld_type = ty} <- universeBi args,
                null (freeTyVars' ty \\ resultVars),
                name <- cd_fld_names
        ]
    ConDeclGADT {con_args = RecCon (L _ args), con_res_ty, con_names} ->
         [ (unLoc con_name, unLoc name, unLoc ty)
         | ConDeclField {cd_fld_names, cd_fld_type = ty} <- universeBi args,
             null (freeTyVars ty \\ freeTyVars con_res_ty),
             name <- cd_fld_names,
             con_name <- con_names
         ]
    _ -> []
    where
        freeTyVars' ty = unLoc <$> freeTyVars ty

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
    = onExp $ setL o $ lhsOp $ rhsApp $ lhsApp $ rhsRec $ mkParen $ mkVar var_getField `mkAppType` sel `mkApp` lhs

-- Turn (.foo.bar) into getField calls
onExp (L o (SectionR _ mid@(isDot -> True) rhs))
    | adjacent mid rhs
    , srcSpanStart o == srcSpanStart (getLoc mid)
    , srcSpanEnd o == srcSpanEnd (getLoc rhs)
    , Just sels <- getSelectors rhs
    -- Don't bracket here. The argument came in as a section so it's
    -- already enclosed in brackets.
    = setL o $ foldl1 (\x y -> noL $ OpApp noE x (mkVar var_dot) y) $ map (mkVar var_getField `mkAppType`) $ reverse sels

-- Turn a{b=c, ...} into setField calls
onExp (L o upd@RecordUpd{rupd_expr,rupd_flds=fld:flds})
    | adjacentBy 1 rupd_expr fld
    = onExp $ f rupd_expr $ fld:flds
    where
        f expr [] = expr
        f expr (L _ (HsRecField (fmap rdrNameAmbiguousFieldOcc -> lbl) arg pun) : flds)
            | let sel = mkSelector lbl
            , let arg2 = if pun then noL $ HsVar noE lbl else arg
            , let expr2 = mkParen $ mkVar var_setField `mkAppType` sel `mkApp` expr `mkApp` arg2  -- 'expr' never needs bracketing.
            = f expr2 flds

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
-- important to copy the location back over, since we check the whitespace hasn't changed
getRec (L l r@RecordUpd{}) = first (\c x -> L l r{rupd_expr=setL (getLoc $ rupd_expr r) $ c x}) $ getRec $ rupd_expr r
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
adjacent = adjacentBy 0

-- | Are the end of a and the start of b next to each other, no white space
adjacentBy :: Int -> Located a -> Located b -> Bool
adjacentBy i (L (realSrcLoc . srcSpanEnd -> Just a) _) (L (realSrcLoc . srcSpanStart -> Just b) _) =
    srcLocFile a == srcLocFile b &&
    srcLocLine a == srcLocLine b &&
    srcLocCol a + i == srcLocCol b
adjacentBy _ _ _ = False


--  Given:
--   C f Int    and     \x -> HasField "field" Entity x
--   Returns:
--   ((C f Int) ~ aplg) => HasField "field" Entity aplg
makeEqQualTy :: HsType GhcPs -> (HsType GhcPs -> HsType GhcPs) -> HsType GhcPs
makeEqQualTy rArg fAbs = HsQualTy noE (noL qualCtx) (noL (fAbs tyVar))
    where
        var = GHC.nameRdrName $ GHC.mkUnboundName $ GHC.mkTyVarOcc "aplg"

        tyVar :: HsType GhcPs
        tyVar = HsTyVar noE GHC.NotPromoted (noL var)

        var_tilde = GHC.mkOrig GHC.gHC_TYPES $ GHC.mkClsOcc "~"

        eqQual :: HsType GhcPs
        eqQual = HsOpTy noE (noL (HsParTy noE (noL rArg))) (noLoc var_tilde) (noLoc tyVar)

        qualCtx :: HsContext GhcPs
        qualCtx = [noL (HsParTy noE (noL eqQual))]
