{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{- HLINT ignore "Use camelCase" -}

-- | Module containing the plugin.
module Compat(module Compat) where

import GHC
#if __GLASGOW_HASKELL__ < 900
import BasicTypes
import TcEvidence
import RnTypes as Compat
import UniqSupply
#else
import GHC.Types.Basic
import GHC.Unit.Types
import GHC.Parser.Annotation
import GHC.Rename.HsType as Compat
import GHC.Types.Unique.Supply
#endif
#if __GLASGOW_HASKELL__ < 810
import HsSyn as Compat
#else
import GHC.Hs as Compat
#endif
#if __GLASGOW_HASKELL__ < 808
import System.IO.Unsafe as Compat (unsafePerformIO)
import TcRnTypes
import IOEnv
import DynFlags
import HscTypes
#endif
import Data.IORef as Compat

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

type PluginEnv = (?hscenv :: HscEnv, ?uniqSupply :: IORef UniqSupply)

dropRnTraceFlags :: HscEnv -> HscEnv
#if __GLASGOW_HASKELL__ < 808
dropRnTraceFlags env@HscEnv{hsc_dflags = dflags} =  env{hsc_dflags = dopt_unset dflags Opt_D_dump_rn_trace}
#else
dropRnTraceFlags = id
#endif

freeTyVars :: PluginEnv => LHsType GhcPs -> [Located RdrName]
#if __GLASGOW_HASKELL__ < 808
{-# NOINLINE freeTyVars #-}
freeTyVars  = freeKiTyVarsAllVars . runRnM . extractHsTyRdrTyVars
  where
    runRnM :: RnM a -> a
    runRnM rnm = unsafePerformIO $ do
      let env = Env ?hscenv ?uniqSupply unused unused
      runIOEnv env rnm
    unused = error "never called"
#elif __GLASGOW_HASKELL__ < 810
freeTyVars = freeKiTyVarsAllVars . extractHsTyRdrTyVars
#else
freeTyVars = extractHsTyRdrTyVars
#endif
