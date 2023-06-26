module Main

data JValue = JNull
            | JBool Bool
            | JNumber Integer -- TODO: support floats
            | JString String
            | JObject [(String, JValue)] -- TODO: this is slow pls fix
            | JNull
            | JArray [JValue]
              deriving (Show, Eq)

main :: IO ()
main = undefined
