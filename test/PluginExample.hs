{-# LANGUAGE CPP #-}

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
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
-- things that are now treated as comments
{-# OPTIONS_GHC -Werror -Wall -Wno-type-defaults -Wno-partial-type-signatures -Wincomplete-record-updates -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures, GADTs, StandaloneDeriving, KindSignatures #-}
module PluginExample where
#include "../examples/Both.hs"

#endif
