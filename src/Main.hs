
module Main(main) where

import Lexer
import Paren
import Unlexer
import Edit
import Control.Monad.Extra
import System.Directory.Extra
import System.Process.Extra
import System.FilePath
import System.IO.Extra
import System.Environment


-- GHC calls me with: original input output <any extra arguments>
-- Test calls me with: --test directory
-- Users call me with: input
main :: IO ()
main = do
    args <- getArgs
    case args of
        "--test":xs -> runTest $ xs ++ ["." | null xs]
        original:input:output:_ -> runConvert original input output
        input:output:_ -> runConvert input input output
        input:_ -> runConvert input input "-"
        [] -> putStrLn "record-dot-preprocess [FILE-TO-CONVERT]"


runConvert :: FilePath -> FilePath -> FilePath -> IO ()
runConvert original input output = do
    res <- unlexer original . unparen . edit . paren . lexer <$> readFileUTF8' input
    if output == "-" then putStrLn res else writeFileUTF8 output res
    where paren = parenOn lexeme [("(",")"),("[","]"),("{","}")]


runTest :: [FilePath] -> IO ()
runTest dirs = withTempDir $ \tdir ->
    forM_ dirs $ \dir -> do
        files <- ifM (doesDirectoryExist dir) (listFilesRecursive dir) (return [dir])
        forM_ files $ \file -> when (takeExtension file `elem` [".hs",".lhs"]) $ do
            let out = tdir </> takeFileName file
            runConvert file file out
            system_ $ "runhaskell \"" ++ out ++ "\""
