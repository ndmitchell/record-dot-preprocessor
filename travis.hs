
import System.Process.Extra
import System.Directory

main = do
    system_ "cabal install microlens"
    createDirectory "Control"
    writeFile "Control/Lens.hs" "module Control.Lens(module X) where import Lens.Micro as X"

    system_ "runhaskell Demo.hs"
    system_ "record-dot-preprocessor --test examples"
