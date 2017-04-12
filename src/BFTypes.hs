module BFTypes where

import ListZipper (BFArray)

type BFProgram = BFArray -> IO BFArray

data Expr 
    = OpInc
    | OpDec
    | OpLeft
    | OpRight
    | IOPrint
    | IOGet
    | Loop [Expr]
    deriving (Show, Eq)

data Token
    = TInc
    | TDec
    | TLeft
    | TRight
    | TPrint
    | TGet
    | TLoopL
    | TLoopR
    deriving (Eq, Show)

tokens = [TLoopL, TLoopR, TInc, TDec, TLeft, TRight, TPrint, TGet]
operatorTokens = drop 2 tokens