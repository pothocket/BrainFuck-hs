module Main where

import Control.Monad.Except (runExceptT)

import Lib

main :: IO ()
main = (runExceptT repl) >>= print