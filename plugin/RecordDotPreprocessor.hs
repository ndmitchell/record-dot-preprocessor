{-# LANGUAGE CPP #-}

module RecordDotPreprocessor(plugin) where

#if __GLASGOW_HASKELL__ < 807
import GHC_8_6
#else
import GHC_HEAD
#endif
