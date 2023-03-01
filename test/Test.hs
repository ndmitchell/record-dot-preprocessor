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
    header <- readFile "examples/Header_in.hs"
    let installed = "--installed" `elem` args
    unless installed $ do
        putStrLn "# PluginExample.hs"
        PluginExample.main
    forM_ (reverse files) $ \file ->
        when (takeExtension file == ".hs" && not ("_out.hs" `isSuffixOf` file) && not ("_in.hs" `isSuffixOf` file)) $ do
            src <- readFile' file
            if installed then do
                forM_ [("Preprocessor", "{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}")
                      ,("Plugin", "{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}")
                      ] $
                    \(name,prefix) -> withTempDir $ \dir ->
                        when (compilerVersion >= makeVersion [8,6] && not (blacklist name (takeBaseName file))) $ do
                            let inp = dir </> takeFileName file
                            putStrLn $ "# " ++ name ++ " " ++ takeFileName file
                            writeFile inp $ prefix ++ "\n" ++ header ++ "\nmodule Main where\n" ++ src
                            system_ $ "runhaskell -package=record-dot-preprocessor " ++ inp
            else withTempDir $ \dir -> do
                let inp = dir </> takeFileName file
                let out = dropExtension file ++ "_out.hs"
                writeFile inp $ header ++ "\n" ++ "module Main where\n" ++ src
                putStrLn $ "# Preprocessor " ++ takeFileName file
                withArgs [file,inp,out] Preprocessor.main
                system_ $ "runhaskell " ++ out
    putStrLn "Success"

-- Blacklist tests we know aren't compatible
blacklist "Plugin" "Preprocessor" = True
blacklist "Plugin" "Both2"        = True
blacklist _ _ = False
