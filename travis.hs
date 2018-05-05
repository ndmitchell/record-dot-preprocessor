
import System.Process.Extra

main = do
    system_ "cabal install microlens"
    system_ "record-dot-preprocessor --test examples"
