module Main where
import Frontend (readLines, EditorInfo (..))
import Frontend.Lexer (lexer)

debugEditor :: EditorInfo
debugEditor = EditorInfo {
    themeIsDark = True,
    tabSize = 4
}

main :: IO ()
main = do
    path <- getLine
    ls <- readLines path
    let toks = lexer debugEditor ls
    print toks
