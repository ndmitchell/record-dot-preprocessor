
import System.Process.Extra

main =
    system_ "chmod go-w .ghci"
    system_ "ghc -e \":test --installed\""
