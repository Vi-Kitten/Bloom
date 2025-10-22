module Frontend (
    EditorInfo (..),
    readLines
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

readLines :: FilePath -> IO [String]
readLines path = do
    text <- Data.Text.Lazy.IO.readFile path
    let ls = text & Data.Text.Lazy.lines
    return $ ls <&> unpack