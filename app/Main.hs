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
import Reporting (runReporter)

debugEditor :: EditorInfo
debugEditor = EditorInfo {
    themeIsDark = True,
    tabSize = 4
}

main :: IO ()
main = do
    putStrLn "what file should I lex?"
    path <- getLine
    ls <- readLines path
    
    -- let (except', events') = runReporter $ debugLexLines debugEditor ls
    -- () <$ mapM print events'
    -- case except' of
    --     Left fatal -> putStrLn "internal compiler error" >> print fatal
    --     Right ls' -> () <$ mapM print ls'

    let (except, events) = runReporter $ processLines debugEditor ls
    () <$ mapM print events
    case except of
        Left fatal -> putStrLn "internal compiler error" >> print fatal
        Right ls' -> () <$ mapM print ls'

