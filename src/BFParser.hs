module BFParser where 

import Data.Maybe (mapMaybe)
import Control.Monad (sequence)
import Data.Char
import Control.Applicative (many, (<|>))
import Control.Monad.Trans.Maybe

import BFTypes
import Parser

tokenMap :: [(Char, Token)]
tokenMap = [ ('+', TInc)
           , ('-', TDec)
           , ('<', TLeft)
           , ('>', TRight)
           , ('.', TPrint)
           , (',', TGet)
           , ('[', TLoopL)
           , (']', TLoopR)
           ]

exprMap :: [(Token, Expr)]
exprMap = [ (TInc, OpInc)
          , (TDec, OpDec)
          , (TLeft, OpLeft)
          , (TRight, OpRight)
          , (TPrint, IOPrint)
          , (TGet, IOGet)
          ]

getTokens :: String -> [Token]
getTokens = mapMaybe . flip lookup $ tokenMap

getExpr :: Token -> Maybe Expr 
getExpr = flip lookup exprMap

operator :: Parser ParseError Token Expr 
operator = parseMaybe $ getExpr <$> anyOf operatorTokens

bfParser :: Parser ParseError Token [Expr]
bfParser = many $ loop bfParser <|> operator

loop :: Parser ParseError Token [Expr] -> Parser ParseError Token Expr
loop p = do
    oneOf TLoopL
    ts <- p
    oneOf TLoopR
    return $ Loop ts