{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Control.Applicative (Alternative (..))
import Data.List (nub)
import Debug.Trace

data Error i e
    = EndOfInput
    | Unexpected i
    | Expected i i
    | CustomError e
    | EmptyError
    | ExpectedEOF i
    deriving (Eq, Show)

newtype Parser i e a = Parser {
    runParser :: [i] -> Either [Error i e] (a, [i])
}

-- i is input stream
-- e is error message
-- a is result of parser

instance Functor (Parser i e) where
    fmap :: (a -> b) -> Parser i e a -> Parser i e b
    fmap f (Parser p) = Parser $ \input -> do
        (output, rest) <- p input
        pure (f output, rest)

instance Applicative (Parser i e) where
    pure :: a -> Parser i e a
    pure a = Parser $ \input -> Right (a, input)

    (<*>) :: Parser i e (a -> b) -> Parser i e a -> Parser i e b
    Parser f <*> Parser p = Parser $ \input -> do
        (f', rest) <- f input
        (output, rest') <- p rest
        pure (f' output, rest')
    

instance (Eq i, Eq e) => Alternative (Parser i e) where
    empty :: (Eq i, Eq e) => Parser i e a
    empty = Parser $ \_ -> Left [EmptyError]

    (<|>) :: (Eq i, Eq e) => Parser i e a -> Parser i e a -> Parser i e a
    Parser l <|> Parser r = Parser $ \input -> 
        case l input of
            Right lout  -> Right lout
            Left lerr   -> case r input of
                Right rout  -> Right rout
                Left  rerr  -> Left $ nub $ lerr <> rerr

instance Monad (Parser i e) where
    return :: a -> Parser i e a
    return = pure

    (>>=) :: Parser i e a -> (a -> Parser i e b) -> Parser i e b
    Parser p >>= k  = Parser $ \input -> do
        (output, rest) <- p input
        runParser (k output) rest

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
    case input of
        [] -> Left [EndOfInput]
        hd : rest   | predicate hd  -> Right (hd, rest)
                    | otherwise     -> Left [Unexpected hd]

char :: Eq i => i -> Parser i e i
char i = Parser $ \input ->
    case runParser (satisfy (== i)) input of
        Right r     -> Right r
        Left err    -> case err of
            [Unexpected c]    -> Left [Expected i c]
            err'              -> Left err'


string :: Eq i => [i] -> Parser i e [i]
string = traverse char


eof :: Show i => Parser i e ()
eof = Parser $ \input ->
        case input of
            []      -> Right ((), [])
            x:_    -> Left [ExpectedEOF x]