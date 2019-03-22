
module Test(main) where

import qualified Preprocessor
import GHC.Records.Extra() -- To ensure the runtime dependency is present

import System.Directory.Extra
import System.Environment
import System.FilePath
import Control.Monad
import System.Process.Extra
import Data.List


main :: IO ()
main = do
    files <- listFiles "examples"
    forM_ files $ \file -> do
        when (takeExtension file == ".hs" && not ("_out.hs" `isSuffixOf` file)) $ do
            let out = dropExtension file ++ "_out.hs"
            withArgs [file,file,out] Preprocessor.main
            system_ $ "runhaskell " ++ out
