module Parser where 

import Control.Monad (ap, liftM)
import Control.Applicative (Alternative(..))
import Data.Monoid ((<>))


------------------------------------------------
-- Types ---------------------------------------

newtype Parser e s a = Parser {parse :: [s] -> Either e (a, [s])}

type ParseError = String

------------------------------------------------
-- Instances -----------------------------------

instance Monad (Parser e s) where 
    return a = Parser $ \str -> Right (a, str)
    p >>= f = Parser $ \xs -> do
        (a, ss) <- parse p xs
        parse (f a) ss
        

instance Applicative (Parser e s) where
    pure = return
    (<*>) = ap

instance Functor (Parser e s) where
    fmap = liftM

instance (Monoid e) => Alternative (Parser e s) where
    empty = Parser $ const (Left mempty)
    p <|> q = Parser $ \str -> 
        case parse p str of
            Right x -> Right x
            Left e1 -> 
                case parse q str of
                    Left e2 -> Left (e1 <> e2)
                    Right b -> Right b
            

-------------------------------------------------

item :: Parser ParseError s s
item = Parser $ \str -> 
    case str of
        []     -> Left "Empty string (item)"
        (x:xs) -> Right (x, xs)

sat :: (Eq s) => (s -> Bool) -> Parser ParseError s s
sat p = Parser f 
    where f str = case str of
                    [] -> Left "Predicate not satisfied (sat)"
                    (x:xs) -> if p x 
                            then Right (x, xs) 
                            else err
                    where err = Left "Predicate not satisfied (sat)"

oneOf :: (Eq s) => s -> Parser ParseError s s
oneOf i = sat (==i)

anyOf :: (Eq s) => [s] -> Parser ParseError s s
anyOf = sat . flip elem

between :: (Eq s) => s -> s -> Parser ParseError s [a] -> Parser ParseError s [a]
between a b p = do
    oneOf a
    xs <- p
    oneOf b
    return xs

parseMaybe :: (Monoid e) => Parser e s (Maybe a) -> Parser e s a
parseMaybe p = do
    r <- p
    case r of
        Just a  -> return a 
        Nothing -> empty