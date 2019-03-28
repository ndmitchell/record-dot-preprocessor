
module GHC_HEAD(plugin) where

import qualified GhcPlugins  as GHC

-- | GHC plugin.
plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.parsedResultAction = \_cliOptions -> pluginImpl
    , GHC.pluginRecompile = GHC.purePlugin
    }


pluginImpl :: GHC.ModSummary -> GHC.HsParsedModule -> GHC.Hsc GHC.HsParsedModule
pluginImpl _modSummary m = return m
