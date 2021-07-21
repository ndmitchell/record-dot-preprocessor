module RecordDotPreprocessor.Lib (module X, runPreprocessStr) where 

import Edit as X (edit, editLoop)
import Lexer as X
import Paren as X

runPreprocessStr :: String -> String
runPreprocessStr input =
    unlexerFile Nothing $ unparens $ editLoop $ paren $ lexer input
    where
        paren = parenOn lexeme [("(",")"),("[","]"),("{","}"),("`","`")]

