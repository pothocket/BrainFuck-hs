module Lib
    ( someFunc
    ) where

import Data.Monoid ((<>))

someFunc :: IO ()
someFunc = getLine >>= putStrLn . \x -> x<>x
