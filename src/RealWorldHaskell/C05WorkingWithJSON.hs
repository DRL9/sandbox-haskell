-- 跟文件名相同
-- 如果没有 () ，那么所有内容都出暴露， 如果都不想暴露， 那么使用 module Name () where
module RealWorldHaskell.C05WorkingWithJSON where

import RealWorldHaskell.C05SimpleJSON (JValue(..))
import RealWorldHaskell.C05PrettyJSON (renderJValue)
import RealWorldHaskell.C05Prettify (pretty,compact)




-- renderJValue :: JValue -> String
-- renderJValue (JString s) = show s
-- renderJValue (JNumber s) = show s
-- renderJValue (JBool True) = "true"
-- renderJValue (JBool False) = "false"
-- renderJValue JNull = "null"
-- renderJValue (JObject obj) = (addBrace . intercalate "," . map renderTuple) obj
--     where renderTuple (key, v) = key ++ ":" ++ (renderJValue v)
--           addBrace str = "{" ++ str ++ "}"
-- renderJValue (JArray list) = (addBracket . intercalate "," . map renderJValue) list
--     where addBracket str = "[" ++ str ++ "]"


test1 = putStrLn $ pretty 2 $ renderJValue $ JBool True
test2 = putStrLn $ pretty 12 $ renderJValue $
    JObject [
                ("a", JArray []),
                ("b", JArray [JNumber 1, JBool False, JString "hello"])
            ]