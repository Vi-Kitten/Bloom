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
import Data.Kind (Type)

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

-- data Parser :: Type -> Type -> Type where
--     FromStrict :: StrictParser t a -> Parser t a
--     Pure :: a -> Parser t a
--     Else :: Parser t a -> a -> Parser t a

-- data StrictParser :: Type -> Type -> Type where
--     Elementary :: (t -> Maybe a) -> StrictParser t a
--     End :: a -> StrictParser t a
--     Prefix :: StrictParser t (a -> b) -> Parser t a -> StrictParser t b
--     Suffix :: Parser t (a -> b) -> StrictParser t a -> StrictParser t b
--     Branch :: StrictParser t a -> StrictParser t a -> StrictParser t a
