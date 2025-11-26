{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sequence_" #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Main where
import Frontend (readLines, EditorInfo (..))
import Frontend.Lexer (debugLexLine)
import Reporting (runReporter)

debugEditor :: EditorInfo
debugEditor = EditorInfo {
    themeIsDark = True,
    tabSize = 4
}

main :: IO ()
-- main = getLine >>= debugLexLine
main = do
    putStrLn "what file should I lex?"
    path <- getLine
    ls <- readLines path
    let (except, events) = runReporter $ mapM debugLexLine ls
    () <$ mapM print events
    case except of
        Left fatal -> putStrLn "internal compiler error" >> print fatal
        Right ls' -> () <$ mapM print ls'
-- main = do
--     putStrLn "what file should I lex?"
--     path <- getLine
--     ls <- readLines path
--     let (except, events) = runReporter $ lexer debugEditor ls
--     () <$ sequence [print event | event <- events]
--     case except of
--         Left fatal -> putStrLn "internal compiler error" >> print fatal
--         Right (Left err) -> putStrLn "lexer error" >> print err
--         Right (Right toks) -> () <$ sequence [print tok | tok <- toks]

