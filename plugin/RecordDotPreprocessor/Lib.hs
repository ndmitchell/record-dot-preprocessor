module RecordDotPreprocessor.Lib (module X, runPreprocessStr) where 

import Edit as X (edit, editLoop)
import Lexer as X
import Paren as X

runPreprocessStr :: String -> String
runPreprocessStr input =
    unlexerFile Nothing $ unparens $ editLoop $ parens $ lexer input

