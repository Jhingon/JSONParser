module Main where

import JSON
import Parser

main :: IO ()
main = readFile "../input.json" >>= parseFile

parseFile :: String -> IO ()
parseFile s = 
  case parsed of
    Left (_, _, r) -> putStrLn $ "Parsing Failed. Remaining String: " ++ r
    Right (a, _) -> print a
  where
    parsed = run parseJSON s
