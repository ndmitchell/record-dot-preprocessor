{-# LANGUAGE CPP #-}
{- HLINT ignore "Use camelCase" -}

-- | Module containing the plugin.
module Compat(module Compat) where

import GHC
#if __GLASGOW_HASKELL__ < 900
import BasicTypes
import TcEvidence
import RnTypes as Compat
#else
import GHC.Types.Basic
import GHC.Unit.Types
import GHC.Parser.Annotation
import GHC.Rename.HsType as Compat
#endif
#if __GLASGOW_HASKELL__ < 810
import HsSyn as Compat
#else
import GHC.Hs as Compat
#endif

---------------------------------------------------------------------
-- UTILITIES

noL :: e -> GenLocated SrcSpan e
noL = noLoc

#if __GLASGOW_HASKELL__ < 810
noE :: NoExt
noE = NoExt
#else
noE :: NoExtField
noE = noExtField
#endif

realSrcLoc :: SrcLoc -> Maybe RealSrcLoc
#if __GLASGOW_HASKELL__ < 811
realSrcLoc (RealSrcLoc x) = Just x
#else
realSrcLoc (RealSrcLoc x _) = Just x
#endif
realSrcLoc _ = Nothing

#if __GLASGOW_HASKELL__ >= 900
hsLTyVarBndrToType :: LHsTyVarBndr flag (GhcPass p) -> LHsType (GhcPass p)
hsLTyVarBndrToType x = noL $ HsTyVar noE NotPromoted $ noL $ hsLTyVarName x
#endif

---------------------------------------------------------------------
-- COMMON SIGNATURES

#if __GLASGOW_HASKELL__ < 811
type Module = HsModule GhcPs
#else
type Module = HsModule
#endif

mkAppType :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkTypeAnn :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkFunTy :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
newFunBind :: Located RdrName -> MatchGroup GhcPs (LHsExpr GhcPs) -> HsBind GhcPs

#if __GLASGOW_HASKELL__ < 807

-- GHC 8.6
mkAppType expr typ = noL $ HsAppType (HsWC noE typ) expr
mkTypeAnn expr typ = noL $ ExprWithTySig (HsWC noE (HsIB noE typ)) expr

#else

-- GHC 8.8+
mkAppType expr typ = noL $ HsAppType noE expr (HsWC noE typ)
mkTypeAnn expr typ = noL $ ExprWithTySig noE expr (HsWC noE (HsIB noE typ))

#endif

#if __GLASGOW_HASKELL__ < 811

-- GHC 8.10 and below
mkFunTy a b = noL $ HsFunTy noE a b
newFunBind a b = FunBind noE a b WpHole []

#else

-- GHC 9.0
mkFunTy a b = noL $ HsFunTy noE (HsUnrestrictedArrow NormalSyntax) a b
newFunBind a b = FunBind noE a b []

#endif


#if __GLASGOW_HASKELL__ < 807

-- GHC 8.6
compat_m_pats :: [Pat GhcPs] -> [LPat GhcPs]
compat_m_pats = map noL

#elif __GLASGOW_HASKELL__ < 809

-- GHC 8.8
compat_m_pats :: [Pat GhcPs] -> [Pat GhcPs]
compat_m_pats = id

#else

-- 8.10
compat_m_pats :: [Pat GhcPs] -> [LPat GhcPs]
compat_m_pats = map noL

#endif


qualifiedImplicitImport :: ModuleName -> LImportDecl GhcPs

#if __GLASGOW_HASKELL__ < 809

-- GHC 8.8
qualifiedImplicitImport x = noL $ ImportDecl noE NoSourceText (noL x) Nothing False False
    True {- qualified -} True {- implicit -} Nothing Nothing

#elif __GLASGOW_HASKELL__ < 811

-- GHC 8.10
qualifiedImplicitImport x = noL $ ImportDecl noE NoSourceText (noL x) Nothing False False
    QualifiedPost {- qualified -} True {- implicit -} Nothing Nothing

#else

-- GHC 9.0
qualifiedImplicitImport x = noL $ ImportDecl noE NoSourceText (noL x) Nothing NotBoot False
    QualifiedPost {- qualified -} True {- implicit -} Nothing Nothing

#endif

#if __GLASGOW_HASKELL__ < 810
freeTyVars = freeKiTyVarsAllVars . extractHsTyRdrTyVars
#else
freeTyVars = extractHsTyRdrTyVars
#endif
