
import System.Process.Extra

main = do
    system_ "cabal install microlens"
    system_ "runhaskell Demo.hs"
    system_ "record-dot-preprocessor --test examples"
