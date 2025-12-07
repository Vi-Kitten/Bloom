{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sequence_" #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Main (
    main
) where
import Frontend (readLines, EditorInfo (..))
import Frontend.Lexer (processLines)
import Frontend.Main (parseFile)
import Reporting (runReporter)
import Frontend.Parser (exampleParser)

debugEditor :: EditorInfo
debugEditor = EditorInfo {
    themeIsDark = True,
    tabSize = 4
}

main :: IO ()
main = do
    putStrLn "what file should I parse?"
    path <- getLine
    res <- parseFile debugEditor path exampleParser
    let (except, events) = runReporter res
    () <$ mapM print events
    case except of
        Left fatal -> putStrLn "internal compiler error" >> print fatal
        Right (Left err) -> putStrLn "parse error" >> print err
        Right (Right x) -> print x

