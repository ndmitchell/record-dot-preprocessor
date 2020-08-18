{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 806

module PluginHkdExample where
main :: IO ()
main = pure ()

#elif mingw32_HOST_OS

module PluginHkdExample where
import RecordDotPreprocessor () -- To check the plugin compiles
main :: IO ()
main = pure ()

#else

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
-- things that are now treated as comments
{-# OPTIONS_GHC -Wall -Wno-type-defaults -Wno-partial-type-signatures -Wincomplete-record-updates -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures, GADTs, StandaloneDeriving, KindSignatures #-}
module PluginHkdExample where
#include "../examples/HkdPlugin.hs"

#endif
