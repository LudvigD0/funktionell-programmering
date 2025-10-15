module Parser where

import Prelude hiding (pure, (<*>), (<|>), (<$>), (<*), (*>))
import Data.Char (isAlpha, isDigit, digitToInt, isSpace)



type Parser a = String -> Maybe (a, String)


parse :: Parser a -> String -> Maybe a
parse p txt = case p txt of 
    Just (x, _) -> Just x
    _           -> Nothing


char :: Char -> Parser Char
char c = sat (== c)
    
sat :: (Char -> Bool) -> Parser Char
sat p = \inp -> case inp of
    x:xs | p x -> Just (x, xs)
    _             -> Nothing


alpha :: Parser Char
alpha = sat isAlpha


choice :: Parser a -> Parser a -> Parser a
choice p q = \inp -> case p inp of
    Just (x,xs) -> Just (x, xs)
    Nothing     -> case q inp of
        Just (x, xs) -> Just (x, xs)
        Nothing -> Nothing 