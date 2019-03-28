
import System.Process.Extra

main =
    system_ "ghc -e \":test --installed\""
