module Frontend (
    EditorInfo (..),
    readLines,
    widthOf
) where

import Data.Text.Lazy (lines, unpack)
import qualified Data.Text.Lazy.IO
import Data.Function ((&))
import Data.Functor ((<&>))
import GHC.TypeLits (Nat)

data EditorInfo = EditorInfo {
    themeIsDark :: Bool,
    tabSize :: Nat
}

widthOf :: Char -> EditorInfo -> Nat
widthOf '\t' i = tabSize i
widthOf '\n' _ = 0
widthOf '\r' _ = 0
widthOf _    _ = 1

readLines :: FilePath -> IO [String]
readLines path = do
    text <- Data.Text.Lazy.IO.readFile path
    let ls = text & Data.Text.Lazy.lines
    return $ ls <&> unpack