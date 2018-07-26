{-# OPTIONS_GHC -W #-}

module RecordDotPlugin(plugin) where

import Control.Monad.IO.Class
import Data.Generics.Uniplate.Data
import Control.Arrow(first)
import Control.Monad

import qualified GhcPlugins  as GHC
import           HsExtension (GhcPs, NoExt(..))
import           HsSyn
import           SrcLoc


noL = L noSrcSpan
mod_ghc_records = GHC.mkModuleName "GHC.Records"
var_getFields = GHC.mkRdrQual mod_ghc_records $ GHC.mkVarOcc "getField"
var_dot = GHC.mkRdrUnqual $ GHC.mkVarOcc "."


plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.parsedResultAction = \_cliOptions -> pluginImpl
    }


pluginImpl :: GHC.ModSummary -> GHC.HsParsedModule -> GHC.Hsc GHC.HsParsedModule
pluginImpl _modSummary m = do
    dflags <- GHC.getDynFlags
    debug $ GHC.showPpr dflags $ GHC.hpm_module m
    -- debug $ SYB.gshow $ GHC.hpm_module m
    let m2 = tweak $ GHC.hpm_module m
    debug $ GHC.showPpr dflags m2
    return m{GHC.hpm_module = m2}


debug :: MonadIO m => String -> m ()
debug = when False . liftIO . putStrLn

tweak :: GHC.Located (HsModule GhcPs) -> GHC.Located (HsModule GhcPs)
tweak = descendBi onExp . transformBi addImports

addImports :: HsModule GhcPs -> HsModule GhcPs
addImports x = x{hsmodImports = magicImport : hsmodImports x}
    where magicImport = noL $ ImportDecl NoExt GHC.NoSourceText (noL mod_ghc_records) Nothing False False True False Nothing Nothing

onExp :: LHsExpr GhcPs -> LHsExpr GhcPs
onExp (L o (OpApp _ lhs mid rhs))
    | adjacent lhs mid, adjacent mid rhs
    , L _ (HsVar _ (L _ mid)) <- mid
    , mid == var_dot
    , L _ (HsVar _ (L _ rhs)) <- rhs
    , not $ GHC.isQual rhs
    , (lhs1, lhs2) <- delve $ onExp lhs
    , let getField = noL $ HsVar NoExt $ noL var_getFields
    , let symbol = HsTyLit NoExt $ HsStrTy GHC.NoSourceText $ GHC.occNameFS $ GHC.rdrNameOcc rhs
    = lhs1 $ noL $ HsPar NoExt $ L o $ HsApp NoExt (noL (HsAppType (HsWC NoExt (noL symbol)) getField)) lhs2
onExp x = descend onExp x


delve :: LHsExpr GhcPs -> (LHsExpr GhcPs -> LHsExpr GhcPs, LHsExpr GhcPs)
delve (L l (OpApp a b c d)) = first (\x -> L l . OpApp a b c . x) $ delve d
delve x = (id, x)


adjacent :: Located a -> Located b -> Bool
adjacent (L (RealSrcSpan a) _) (L (RealSrcSpan b) _) =
    srcSpanEndLine a == srcSpanStartLine b &&
    srcSpanEndCol a == srcSpanStartCol b
adjacent _ _ = False
