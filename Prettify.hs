module Prettify where

import JSON
import Utility

prettify :: Int -> JValue -> String
prettify n (JObject o) = prObj n (JObject o)
prettify n (JArray a)  = prArray n a
prettify _ (JString s) = prString s
prettify _ (JNumber num) = prNumber num
prettify _ (JBool b)   = prBool b
prettify _ (JNull)     = "null"

prObj :: Int -> JValue -> String
prObj n (JObject (kv:kvs)) = "{\n" ++ pair kv ++ pairs ++ "\n" ++ indent n ++ "}"
  where
    pair (key, value) =
      indent (n + 1) ++ "\"" ++ key ++ "\"" ++ ": " ++ prettify (n+1) value
    pairs = foldl (\acc x -> acc ++ prPair x) "" kvs
    prPair p = ",\n" ++ pair p

prArray :: Int -> [JValue] -> String
prArray n a = "[\n" ++ (foldl g "" a) ++ indent n ++ "]" 
  where
    g acc x = acc ++ prettify (n+1) x ++ ",\n" 

prString :: String -> String
prString s = "\"" ++ s ++ "\""

prNumber :: Number -> String
prNumber (I n) = show n
prNumber (F n) = show n
prNumber (Sci (Sc sc e)) = showSc sc ++ "e" ++ show e
  where
    showSc (Si n) = show n
    showSc (Sf n) = show n

prBool :: Bool -> String
prBool b = if b then "true" else "false"

indent :: Int -> String
indent n = join $ take n $ (repeat "\t")
