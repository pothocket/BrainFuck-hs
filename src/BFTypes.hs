module BFTypes where

type BFProgram = [Expr]

data Expr 
    = OpInc
    | OpDec
    | OpLeft
    | OpRight
    | IOPrint
    | IOGet
    | Loop [Expr]
    deriving (Show)

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