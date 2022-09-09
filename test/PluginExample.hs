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
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies, TypeOperators, GADTs, UndecidableInstances, RankNTypes #-}
-- things that are now treated as comments
{-# OPTIONS_GHC -Werror -Wall -Wno-type-defaults -Wno-partial-type-signatures -Wno-incomplete-record-updates -Wno-unused-top-binds #-}
#if __GLASGOW_HASKELL__ >=902
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
#endif
{-# LANGUAGE PartialTypeSignatures, GADTs, StandaloneDeriving, KindSignatures #-}
#if __GLASGOW_HASKELL__ < 808
-- 8.8+ doesn't need it to be set explicitly
{-# LANGUAGE ExistentialQuantification #-}
#endif

{-# LANGUAGE RankNTypes #-}
module PluginExample where
#include "../examples/Both.hs"

data PolyField = PolyField
    { polyField :: forall a. a -> IO ()
    }



#endif
