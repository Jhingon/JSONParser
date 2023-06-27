module Main where

data JValue 
  = JNull
  | JBool Bool
  | JNumber Integer -- TODO: support floats
  | JString String
  | JObject [(String, JValue)] -- TODO: this is slow pls fix
  | JArray [JValue]
  deriving (Show, Eq)

main :: IO ()
main = undefined

newtype Parser a = Parser { run :: String -> Either (Int, Int, String) (a, String) }

parseJValue :: Parser JValue
parseJValue = undefined

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


class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  some :: f a -> f [a]
  some fa = (:) <$> fa <*> many fa 

  many :: f a -> f [a]
  many fa = some fa <|> pure []

instance Alternative Parser where
  empty = Parser (\s -> Left (0, 0, ""))
  (Parser pa) <|> (Parser pb) = Parser (\s -> (pa s) <> (pb s))

satisfy :: (Char -> Bool) -> Parser Char
satisfy g = Parser run
  where
    run [] = Left (0, 0, "")
    run (c:s)
      | g c = Right (c, s)
      | otherwise = Left (0, 0, s)


parseInt :: Parser Integer 
parseInt = Parser run
  where
    run xs 
      | null digits = Left (0, 0, xs)
      | otherwise = Right (read digits, s)
      where 
        (digits, s) = span (isDigit) xs

spaces :: Parser String
spaces = many (satisfy isSpace)

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _   = False

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace '\r' = True
isSpace _    = False
