module Test(main) where

import qualified Preprocessor
import qualified PluginExample -- To test the plugin
import GHC.Records.Extra() -- To ensure the runtime dependency is present

import System.Directory.Extra
import System.Environment
import System.FilePath
import Control.Monad
import System.IO.Extra
import System.Info
import System.Process.Extra
import Data.List
import Data.Version


main :: IO ()
main = do
    -- TODO: If you pass `--installed` should create temp files with the magic string in front
    args <- getArgs
    files <- listFiles "examples"
    let installed = "--installed" `elem` args
    unless installed $ do
        putStrLn "# PluginExample.hs"
        PluginExample.main
    forM_ (reverse files) $ \file ->
        when (takeExtension file == ".hs" && not ("_out.hs" `isSuffixOf` file)) $
            if installed then do
                src <- readFile' file
                forM_ [("Preprocessor", "{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}")
                      ,("Plugin", "{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}\n" ++
                                  "{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}")] $
                    \(name,prefix) -> withTempDir $ \dir ->
                        when (compilerVersion >= makeVersion [8,6] && not (blacklist name (takeBaseName file))) $ do
                            putStrLn $ "# " ++ name ++ " " ++ takeFileName file
                            writeFile (dir </> takeFileName file) $ prefix ++ "\n" ++ src
                            system_ $ "runhaskell -package=record-dot-preprocessor " ++ dir </> takeFileName file
            else do
                let out = dropExtension file ++ "_out.hs"
                putStrLn $ "# Preprocessor " ++ takeFileName file
                withArgs [file,file,out] Preprocessor.main
                system_ $ "runhaskell " ++ out
    putStrLn "Success"

-- Blacklist tests we know aren't compatible
blacklist "Plugin" "Preprocessor" = True
blacklist _ _ = False
