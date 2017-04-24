module Eval (evalBF, initArray) where 

import Control.Applicative
import Control.Monad ((>=>))

import ListZipper
import BFTypes
import BFParser

initArray :: BFArray
initArray = mkZipper 0

dispatch :: ([Expr] -> BFProgram) -> Expr -> BFProgram
dispatch e expr = case expr of
    OpInc   -> return <$> modify succ
    OpDec   -> return <$> modify (subtract 1)
    OpLeft  -> return <$> zLeft
    OpRight -> return <$> zRight
    IOGet   -> \xs -> do
        n <- getLine
        return $ set (read n) xs
    IOPrint -> liftA2 (>>) (putStrLn . show . getCursor) return
    Loop xs -> e xs

evalBF :: [Expr] -> BFArray -> IO BFArray
evalBF = foldr1 (>=>) . fmap (dispatch evalLoop)

evalLoop :: [Expr] -> BFProgram
evalLoop xs t
    | getCursor t == 0 = return t
    | otherwise        = evalLoop xs =<< (evalBF xs t)