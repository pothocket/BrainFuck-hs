module Parser where 

import Data.Maybe (mapMaybe)
import Control.Monad (sequence)
import Data.Char

import BFTypes

type ParseError = String

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

parseBF :: String -> [Token]
parseBF = mapMaybe getToken

parseTokens :: [Token] -> Either ParseError BFProgram
parseTokens [] = Right []
parseTokens (t:ts) = Right (Op OpLeft) :  parseTokens ts