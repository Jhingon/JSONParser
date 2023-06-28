module Parser where

import Utility

newtype Parser a = Parser { run :: String -> Either (Int, Int, String) (a, String) }

instance Functor Parser where
  fmap g pa = Parser runPb
    where runPb s = first g <$> ((run pa) s)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Applicative Parser where
  pure a =  Parser (\s -> Right (a, s))
  (Parser pg) <*> (Parser pa) = Parser pb
    where
      pb s = (pg s) >>= (\(g, str) -> (first g) <$> (pa str))

instance Alternative Parser where
  empty = Parser (\s -> Left (0, 0, ""))
  (Parser pa) <|> (Parser pb) = Parser (\s -> (pa s) <> (pb s))
