module Lib
    ( repl
    ) where

import Eval
import BFParser
import Parser
import ListZipper

import Control.Monad.Except

e = ExceptT . return --why

repl :: ExceptT ParseError IO BFArray
repl = do
    input <- liftIO getLine
    program <- e $ fst <$> parse bfParser (getTokens input)
    liftIO $ evalBF program initArray 