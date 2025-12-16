module Frontend.Main (
    lexFile,
    parseFile
) where
import Frontend.Parser (Parser, ParseError, parse)
import Reporting (CompilerExcept)
import Frontend (readLines, widthOf, EditorInfo (..))
import Frontend.Lexer (processLines, Token)
import Frontend.Spanned (TextPos (..), Spanned)
import Utils (len, lastIn)
import Data.Function ((&))

lexFile :: EditorInfo -> FilePath -> IO (TextPos, CompilerExcept [Spanned Token])
lexFile i path = do
    ls <- readLines path
    let maybe_final = lastIn ls
    let end = case maybe_final of
            Nothing -> TextPos 0 0
            Just final -> TextPos (len ls) $ sum [i & widthOf t | t <- final]
    return (end, processLines i ls)

parseFile :: EditorInfo -> FilePath -> Parser a -> IO (CompilerExcept (Either ParseError a))
parseFile i path px = do
    (end, mtoks) <- lexFile i path
    return $ do
        toks <- mtoks
        parse end toks px