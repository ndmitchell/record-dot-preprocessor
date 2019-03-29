
import System.Process.Extra

main = do
    system_ "chmod go-w .ghci"
    system_ "chmod go-w ."
    system_ "ghc -e \":test --installed\""
