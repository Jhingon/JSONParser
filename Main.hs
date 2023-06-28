module Main where

data JValue 
  = JNull
  | JBool Bool
  | JNumber Number
  | JString String
  | JObject [(String, JValue)] -- TODO: this is slow pls fix
  | JArray [JValue]
  deriving (Show, Eq)

data Number = I Integer | F Float | Sci Scientific
  deriving (Show, Eq)

data Scientific = Sc Significant Integer
  deriving (Show, Eq)

data Significant = Sf Float | Si Integer 
  deriving (Show, Eq)

main :: IO ()
main = undefined

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

chars :: String -> Parser String
chars (s : []) = satisfy (==s) <:> pure []
chars (s:ss) = satisfy (==s) <:> chars ss

digits :: Parser String
digits = many (satisfy isDigit)

posInt :: Parser Integer 
posInt = Parser run
  where
    run xs 
      | null ds = Left (0, 0, xs)
      | otherwise = Right (read ds, s)
      where 
        (ds, s) = span (isDigit) xs

negInt :: Parser Integer
negInt = fmap negate (satisfy (=='-') *> posInt)

parseInt :: Parser Integer
parseInt = posInt <|> negInt

parseFloat :: Parser Float
parseFloat = (\x -> (read x)) <$> (digits <++> (satisfy (=='.') <:> pure []) <++> digits)

parseScientific :: Parser Scientific
parseScientific = Sc 
  <$> ((Sf <$> parseFloat) <|> (Si <$> parseInt)) 
  <*> ((satisfy (\c -> c == 'e' || c == 'E') *> parseInt))

parseNumber :: Parser JValue
parseNumber =  JNumber 
  <$> (Sci <$> parseScientific)
  <|> (F   <$> parseFloat) 
  <|> (I   <$> parseInt) 

parseString :: Parser JValue
parseString = 
  JString 
  <$> 
    ( (satisfy (=='\"') <:> pure []) <++> (
     (  many acceptable) 
    <|> escape '\"'
    <|> escape '\\'
    <|> escape '/'
    <|> escape 'b'
    <|> escape 'f'
    <|> escape 'n'
    <|> escape 'r'
    <|> escape 't') <++> (satisfy (=='\"') <:> pure [])
  )
  where
    -- TODO: Add \u
    escape c   = satisfy (=='\\') <:> satisfy (==c) <:> pure []
    acceptable = satisfy (/= '\"')

parseNull :: Parser JValue
parseNull = const JNull <$> chars "null"

parseBool :: Parser JValue
parseBool = JBool <$> (const True <$> chars "true") <|> (const False <$> chars "false")

parsePair :: Parser (String, JValue)
parsePair = (,) <$> (convert <$> key) <*> value
  where
    key = parseString <* whitespace <* satisfy (==':')
    value = parseJSON
    convert (JString s) = s

parseObject :: Parser JValue
parseObject 
  = JObject 
  <$> (whitespace *> satisfy (=='{') *> parsePair <:> rest <* whitespace <* satisfy (=='}'))
    where
      rest = many (satisfy (==',') *> parsePair)

parseArray :: Parser JValue
parseArray = JArray <$> (satisfy (=='[') *> (parseJSON <:> rest) <* satisfy (==']'))
  where 
    rest = many (satisfy (==',') *> parseJSON)

parseJSON :: Parser JValue
parseJSON =  
  (whitespace *> 
      (   parseObject 
      <|> parseArray
      <|> parseString 
      <|> parseNumber 
      <|> parseBool 
      <|> parseNull) <* whitespace)

whitespace :: Parser String
whitespace = many (satisfy isSpace)

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

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g fa fb = g <$> fa <*> fb

infixr 3 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

infixr 3 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)
