
import System.Process.Extra
import System.Directory

main = do
    system_ "cabal install record-hasfields"
    system_ "runhaskell Demo.hs"
    system_ "record-dot-preprocessor --test examples"
    system_ "runhaskell -F -pgmF=record-dot-preprocessor -isrc src/Main.hs"
