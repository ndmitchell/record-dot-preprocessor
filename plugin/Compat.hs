{-# LANGUAGE CPP #-}
{- HLINT ignore "Use camelCase" -}

-- | Module containing the plugin.
module Compat(module Compat) where

import qualified GHC
import HsSyn
import SrcLoc

---------------------------------------------------------------------
-- UTILITIES

noL :: e -> GenLocated SrcSpan e
noL = noLoc

noE :: GHC.NoExt
noE = GHC.NoExt


---------------------------------------------------------------------
-- COMMON SIGNATURES

mkAppType :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkTypeAnn :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs


#if __GLASGOW_HASKELL__ < 807

---------------------------------------------------------------------
-- GHC 8.6

mkAppType expr typ = noL $ HsAppType (HsWC noE typ) expr
mkTypeAnn expr typ = noL $ ExprWithTySig (HsWC noE (HsIB noE typ)) expr

compat_m_pats :: [Pat GhcPs] -> [LPat GhcPs]
compat_m_pats = map noL

#else

---------------------------------------------------------------------
-- GHC HEAD

mkAppType expr typ = noL $ HsAppType noE expr (HsWC noE typ)
mkTypeAnn expr typ = noL $ ExprWithTySig noE expr (HsWC noE (HsIB noE typ))

compat_m_pats :: [Pat GhcPs] -> [Pat GhcPs]
compat_m_pats = id

#endif
