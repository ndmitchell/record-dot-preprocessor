{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

#if __GLASGOW_HASKELL__ < 806

module PluginExample where
main :: IO ()
main = pure ()

#elif mingw32_HOST_OS

module PluginExample where
import RecordDotPreprocessor() -- To check the plugin compiles
main :: IO ()
main = pure ()

#else

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

#include "../examples/Header_in.hs"

module PluginExample where

#include "../examples/Both.hs"

data PolyField = PolyField
    { polyField :: forall a. a -> IO ()
    }

data PolyGieldGADTs where
    PolyFieldGADTs :: { a' :: forall a. a } -> PolyGieldGADTs

#endif
