module BFTypes where

type BFProgram = [Expr]

-- Expr --
data Expr 
    = Op Op
    | Loop [Expr]
    | IO' IOType

type AST = [Expr]
    
data Op
    = OpInc
    | OpDec
    | OpLeft
    | OpRight
    deriving Eq

data IOType
    = IOIn
    | IOOut
    deriving Eq

-- Tokens --
data Token
    = TInc
    | TDec
    | TLeft
    | TRight
    | TPrint
    | TGet
    | TLoopL
    | TLoopR
    deriving Eq

tokens = [TLoopL, TLoopR, TInc, TDec, TLeft, TRight, TPrint, TGet]
operatorTokens = drop 2 tokens