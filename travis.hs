
import Control.Monad
import Data.List
import System.Exit
import System.Process.Extra

main = do
    system_ "chmod go-w .ghci"
    system_ "chmod go-w ."
    (code, xs) <- systemOutput "ghc -e \":test --installed\""
    putStrLn xs
    when (code /= ExitSuccess || not ("Success" `isInfixOf` xs)) $
        putStrLn "Running the test did not succeed"
