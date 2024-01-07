-- 跟文件名相同
-- 如果没有 () ，那么所有内容都出暴露， 如果都不想暴露， 那么使用 module Name () where
module RealWorldHaskell.C05SimpleJSON (
    JValue(..) -- .. 表示导出这个类型以及其构造器
    )where



data JValue = JString String
            | JNumber Double
            | JNull
            | JBool Bool
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Show, Eq, Ord)

getString (JString s) = Just s
getString _ = Nothing
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v = v == JNull
