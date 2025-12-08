module Frontend.Main (parseFile) where
import Frontend.Parser (Parser, ParseError, parse)
import Reporting (CompilerExcept)
import Frontend (readLines, EditorInfo)
import Frontend.Lexer (processLines)
import Frontend.Spanned (TextPos(..))
import Utils (len)

parseFile :: EditorInfo -> FilePath -> Parser a -> IO (CompilerExcept (Either ParseError a))
parseFile i path px = do
    ls <- readLines path
    return $ do
        ls' <- processLines i ls
        parse (TextPos (len ls) 0) ls' px