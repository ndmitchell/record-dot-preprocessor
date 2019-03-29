
import Control.Monad
import Data.List
import System.Process.Extra

main = do
    system_ "chmod go-w .ghci"
    system_ "chmod go-w ."
    xs <- systemOutput_ "ghc -e \":test --installed\""
    putStrLn xs
    unless ("Success" `isInfixOf` xs) $
        putStrLn "Running the test did not succeed"
