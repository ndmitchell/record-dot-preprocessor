{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Unused LANGUAGE pragma" -}

-- | Module containing the plugin.
module Compat(module Compat) where

import GHC
#if __GLASGOW_HASKELL__ > 901
import GHC.Types.SourceText ( SourceText(NoSourceText) )
import GHC.Data.FastString (FastString, NonDetFastString (NonDetFastString))
#elif __GLASGOW_HASKELL__ >=900
import GHC.Data.FastString (FastString)
#else
import FastString (FastString)
#endif

#if __GLASGOW_HASKELL__ < 900
import BasicTypes
import TcEvidence
import RnTypes as Compat
import UniqSupply
#else
import GHC.Types.Basic
import GHC.Unit.Types
#if __GLASGOW_HASKELL__ < 902
import GHC.Parser.Annotation
#endif
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
#if __GLASGOW_HASKELL__ >= 904
import GHC.Types.PkgQual (RawPkgQual(NoRawPkgQual))
#endif
import Data.IORef as Compat

---------------------------------------------------------------------
-- LOCATIONS

class WithoutLoc a b | b -> a where
  -- | Without location information
  --
  -- Different GHC versions want different kind of location information in
  -- different places. This class is intended to abstract over this.
  noL :: a -> b

#if __GLASGOW_HASKELL__ >= 902
instance WithoutLoc a (GenLocated (SrcAnn ann) a) where
  noL = reLocA . noLoc
#endif

instance WithoutLoc a (Located a) where
  noL = noLoc

instance WithoutLoc (HsTupArg p)         (HsTupArg p)         where noL = id
instance WithoutLoc (HsLocalBindsLR p q) (HsLocalBindsLR p q) where noL = id

#if __GLASGOW_HASKELL__ < 902
reLocA :: Located e -> Located e
reLocA = id

reLoc :: Located e -> Located e
reLoc = id
#endif

---------------------------------------------------------------------
-- TREE EXTENSIONS

class WithoutExt a where
  -- | No extension
  --
  -- Different GHC versions want different kinds of annotations. This class is
  -- intended to abstract over this.
  noE :: a

#if __GLASGOW_HASKELL__ >= 902
instance WithoutExt (EpAnn a) where
  noE = EpAnnNotUsed

instance WithoutExt EpAnnComments where
  noE = emptyComments
#endif

#if __GLASGOW_HASKELL__ >= 810
instance WithoutExt NoExtField where
  noE = noExtField
#else
instance WithoutExt NoExt where
  noE = NoExt
#endif

---------------------------------------------------------------------
-- UTILITIES

#if __GLASGOW_HASKELL__ < 902

mkNonDetFastString :: FastString -> FastString
mkNonDetFastString = id

#else

mkNonDetFastString :: FastString -> NonDetFastString
mkNonDetFastString = NonDetFastString

#endif

realSrcLoc :: SrcLoc -> Maybe RealSrcLoc
#if __GLASGOW_HASKELL__ < 811
realSrcLoc (RealSrcLoc x) = Just x
#else
realSrcLoc (RealSrcLoc x _) = Just x
#endif
realSrcLoc _ = Nothing

#if __GLASGOW_HASKELL__ >= 902
hsLTyVarBndrToType :: (Anno (IdP (GhcPass p)) ~ SrcSpanAnn' (EpAnn NameAnn)) => LHsTyVarBndr flag (GhcPass p) -> LHsType (GhcPass p)
hsLTyVarBndrToType x = noL $ HsTyVar noE NotPromoted $ noL $ hsLTyVarName x
#elif __GLASGOW_HASKELL__ >= 900
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

#elif __GLASGOW_HASKELL__ < 901

-- GHC 8.8-9.0
mkAppType expr typ = noL $ HsAppType noE expr (HsWC noE typ)
mkTypeAnn expr typ = noL $ ExprWithTySig noE expr (HsWC noE (HsIB noE typ))

#else

-- GHC 9.2+
mkAppType expr typ = noL $ HsAppType noSrcSpan expr (HsWC noE typ)
mkTypeAnn expr typ = noL $ ExprWithTySig noE expr (hsTypeToHsSigWcType typ)

#endif

#if __GLASGOW_HASKELL__ < 811

-- GHC 8.10 and below
mkFunTy a b = noL $ HsFunTy noE a b
newFunBind a b = FunBind noE a b WpHole []

#elif __GLASGOW_HASKELL__ < 904

-- GHC 9.0 and 9.2
mkFunTy a b = noL $ HsFunTy noE (HsUnrestrictedArrow NormalSyntax) a b
newFunBind a b = FunBind noE (reLocA a) b []

#else

-- GHC >= 9.4
mkFunTy a b = noL $ HsFunTy noE (HsUnrestrictedArrow $ L NoTokenLoc HsNormalTok) a b
newFunBind a b = FunBind noE (reLocA a) b []

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

#elif __GLASGOW_HASKELL__ < 904

-- GHC 9.0 and 9.2
qualifiedImplicitImport x = noL $ ImportDecl noE NoSourceText (noL x) Nothing NotBoot False
    QualifiedPost {- qualified -} True {- implicit -} Nothing Nothing

#else

-- GHC >= 9.4
qualifiedImplicitImport x = noL $ ImportDecl noE NoSourceText (noL x) NoRawPkgQual NotBoot False
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
freeTyVars = map reLoc . extractHsTyRdrTyVars
#endif

#if __GLASGOW_HASKELL__ >= 902
isLHsForAllTy :: LHsType GhcPs -> Bool
isLHsForAllTy (L _ (HsForAllTy {})) = True
isLHsForAllTy _                     = False
#endif

#if __GLASGOW_HASKELL__ >= 904
rdrNameFieldOcc :: FieldOcc GhcPs -> LocatedN RdrName
rdrNameFieldOcc = foLabel
#endif
