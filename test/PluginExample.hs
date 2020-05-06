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

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -w #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures, GADTs, StandaloneDeriving, KindSignatures #-} -- because it's now treated as a comment
module PluginExample where
#include "../examples/Both.hs"

#endif
