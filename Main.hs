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

-- TODO: Add support for pointing out where the error if 
-- Kinda like: run :: Either (Int, Int, String) (a, String)
newtype Parser a = Parser { run :: String -> Either (Int, Int, String) (a, String)}

parseJValue :: Parser JValue
parseJValue = undefined

instance Functor Parser where
  fmap g pa = Parser runPb
    where runPb s = first g <$> (run pa) s

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Applicative Parser where
  pure a =  Parser (\s -> Right (a, s))
  (Parser pg) <*> (Parser pa) = Parser pb
    where
      pb s = (pg s) >>= \g -> fmap (first $ fst g) (pa s)
