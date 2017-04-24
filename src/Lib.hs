module Lib
    ( repl
    ) where

import Eval
import BFParser
import Parser
import ListZipper

import Control.Monad.Except

import System.IO (hSetBuffering, stdin, stdout, BufferMode (..))

e = ExceptT . return --why

repl :: IO ()
repl = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStrLn "Enter BF Program: "
    result <- runExceptT promptProgram
    case result of
        Left e -> putStrLn ("Error: " ++ e)
        Right r -> print r
    putStrLn "Quit? (y/n)"
    input <- getLine
    if input == "y" then return () else repl

promptProgram :: ExceptT ParseError IO BFArray
promptProgram = do
    input <- liftIO getLine
    program <- e $ fst <$> parse bfParser (getTokens input)
    liftIO $ evalBF program initArray

                