module BFParser where 

import Data.Maybe (mapMaybe)
import Control.Monad (sequence)
import Data.Char

import BFTypes
import Parser

getToken :: Char -> Maybe Token
getToken c = case c of
    '+' -> Just TInc
    '-' -> Just TDec
    '<' -> Just TLeft
    '>' -> Just TRight
    '.' -> Just TPrint
    ',' -> Just TLeft
    '[' -> Just TLoopL
    ']' -> Just TLoopR
    _   -> Nothing

operator :: Parser ParseError Token Token
operator = anyOf operatorTokens

bfParser :: Parser ParseError Token [Expr]