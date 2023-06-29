module Main where

import JSON
import Parser
import Prettify

main :: IO ()
main = readFile "../input.json" >>= parseFile

parseFile :: String -> IO ()
parseFile s = 
  case parsed of
    Left (_, _, r) -> putStrLn $ "Parsing Failed. Remaining String: " ++ r
    Right (a, _) -> writeFile "../output.json" $ prettify 0 a
  where
    parsed = run parseJSON s
