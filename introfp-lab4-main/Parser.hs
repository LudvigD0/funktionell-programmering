{- |
Module      : Parser
Description : A simple parser combinator library.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module Parser
  ( Parse
  , parse
  , sat, alpha, char
  , many, many1, sepby, sepby1, (<|>), chainr, chainl, opt
  , string, whiteSpace, trim, within, parens
  , nat, int, double
  ) where

import Data.Char

newtype Parse a = P (String -> Maybe (a, String))

instance Functor Parse where
  fmap f (P p) = P $ \inp -> fmap (\(x, s) -> (f x, s)) (p inp)

instance Applicative Parse where
  pure x      = P $ \inp -> Just (x, inp)
  P f <*> P p = P $ \inp -> do
    (f, xs) <- f inp
    (x, ys) <- p xs
    return (f x, ys)

parse :: Parse a -> String -> Maybe a
parse (P p) inp = fst <$> p inp

sat :: (Char -> Bool) -> Parse Char
sat p = P $ \inp -> case inp of
  c:cs | p c -> Just (c, cs)
  _          -> Nothing

alpha :: Parse Char
alpha = sat isAlpha

char :: Char -> Parse Char
char c = sat (== c)

whiteSpace :: Parse String
whiteSpace = many (sat isSpace)

(<|>) :: Parse a -> Parse a -> Parse a
P p <|> P q = P $ \inp -> case p inp of
  Just x -> Just x
  _      -> q inp

infixl 3 <|>

trim :: Parse a -> Parse a
trim p = whiteSpace *> p <* whiteSpace

double :: Parse Double
double = read . concat <$> sequenceA [sign, digits, fraction, exp]
 where
  digits   = many1 (sat isDigit)
  try p    = opt p ""
  sign     = try (string "-")
  fraction = try ((:) <$> char '.' <*> digits)
  exp      = try ((:) <$> (char 'e' <|> char 'E') <*> digits)

within :: (Char, Char) -> Parse a -> Parse a
within (open, close) p = char open *> p <* char close

parens :: Parse a -> Parse a
parens = within ('(', ')')

string :: String -> Parse String
string = sequenceA . map char

many, many1 :: Parse a -> Parse [a]
many  p = (:) <$> p <*> many p <|> pure []
many1 p = (:) <$> p <*> many p

opt :: Parse a -> a -> Parse a
p `opt` x = p <|> pure x

sepby, sepby1 :: Parse a -> Parse b -> Parse [a]
sepby  p sep = (:) <$> p <*> many (sep *> p) <|> pure []
sepby1 p sep = (:) <$> p <*> many (sep *> p)

nat, int :: Parse Int
nat = read <$> many1 (sat isDigit)
int = (negate <$ char '-') `opt` id <*> nat

chainr :: Parse a -> Parse (a -> a -> a) -> Parse a
chainr p op = app <$> many ((\x f -> f x) <$> p <*> op) <*> p
 where
  app = flip (foldr ($))

chainl :: Parse a -> Parse (a -> a -> a) -> Parse a
chainl p op = app <$> p <*> many (flip <$> op <*> p)
 where
  app = foldl (flip ($))
