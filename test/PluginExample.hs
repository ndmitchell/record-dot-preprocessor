{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -w #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds #-}
module PluginExample where
#include "../examples/Example.hs"
