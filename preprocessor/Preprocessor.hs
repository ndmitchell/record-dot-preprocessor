
module Preprocessor(main) where

import Lexer
import Paren
import Edit
import System.IO.Extra
import System.Environment


-- GHC calls me with: original input output <any extra arguments>
-- Test calls me with: --test directory
-- Users call me with: input
main :: IO ()
main = do
    args <- getArgs
    case args of
        original:input:output:_ -> runConvert original input output
        input:output:_ -> runConvert input input output
        input:_ -> runConvert input input "-"
        [] -> putStrLn "record-dot-preprocess [FILE-TO-CONVERT]"


runConvert :: FilePath -> FilePath -> FilePath -> IO ()
runConvert original input output = do
    res <- unlexerFile (Just original) . unparens . edit . parens . lexer <$> readFileUTF8' input
    if output == "-" then putStrLn res else writeFileUTF8 output res
