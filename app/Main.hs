{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sequence_" #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (
    main
) where
import Frontend (readLines, EditorInfo (..))
import Frontend.Main (parseFile, lexFile)
import Reporting (runReporter)
import Frontend.Parser (parse, expr, curlyItem)

debugEditor :: EditorInfo
debugEditor = EditorInfo {
    themeIsDark = True,
    tabSize = 4
}

main :: IO ()
main = do
    putStrLn "what file should I parse?"
    path <- getLine
    (end, mtoks) <- lexFile debugEditor path
    let (except, events) = runReporter $ do
            toks <- mtoks
            res <- parse end toks $ curlyItem *> expr
            return (toks, res)
    () <$ mapM print events
    case except of
        Left fatal -> putStrLn "internal compiler error" >> print fatal
        Right (toks, Left err) -> putStrLn "tokens"
            >> sequenceA [print tok | tok <- toks]
            >> putStrLn "parse error"
            >> print err
        Right (toks, Right x) -> putStrLn "tokens"
            >> sequenceA [print tok | tok <- toks]
            >> putStrLn "parse success"
            >> print x
