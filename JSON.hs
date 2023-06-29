module JSON where

import Parser
import Utility

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

whitespace :: Parser String
whitespace = many (satisfy isSpace)

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
parseFloat = read <$> ((satisfy (=='-') <:> parseDigits) <|> parseDigits)
  where
    parseDigits = digits <++> (satisfy (=='.') <:> digits)

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
    (satisfy (=='\"') *> (
     (  many acceptable) 
    <|> escape '\"'
    <|> escape '\\'
    <|> escape '/'
    <|> escape 'b'
    <|> escape 'f'
    <|> escape 'n'
    <|> escape 'r'
    <|> escape 't') <* satisfy (=='\"')
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
    key = whitespace *> parseString <* whitespace <* satisfy (==':')
    value = parseJSON
    convert (JString s) = s

parseObject :: Parser JValue
parseObject 
  = JObject 
  <$> (whitespace *> satisfy (=='{') *> (parsePair <:> rest) <* whitespace <* satisfy (=='}'))
    where
      rest = many (satisfy (==',') *> whitespace *> parsePair)

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

