module RealWorldHaskell.C08Regex where


-- 所有正则库都依赖 Text.Regex.Base

-- POSIX 实现
import Text.Regex.Posix ( (=~) )

-- regex-tdfa 更好的性能的实现
-- regex-pcre perl-style 的实现

-- 左边是字符串， 右边是正则表达式
-- 需要指定返回类型
test1 = "hello" =~ "[a-z]+" :: Bool
test2 = "hello world" =~ "[a-z]+" :: Bool
test3 = "hello world" =~ "^[a-z]+$" :: Bool

-- 返回 Int, 返回匹配次数
test4 = "12_345" =~ "[1-9]+" :: Int

-- 返回 String, 则是匹配到的第一个结果, 如果没有匹配到，则是 ""
test5 = "hello world" =~ "[a-z]+" :: String

-- 三元组
-- 1: 匹配目标之前的字符串， 2: 第一个匹配的字符串， 3: 匹配目标之后的字符串
test6 = " hello world " =~ "([a-z]+)" :: (String,String,String)

-- 四元组
-- 4: 所有匹配组
test7 = " hello world " =~ "h([a-z])l([a-z]+)" :: (String,String,String, [String])

-- 匹配开始位置， 匹配字符串的长度
test8 = " hello world" =~ "[a-z]+" :: (Int, Int)
-- 匹配失败，则为 (-1, 0)
test9 = " hello world" =~ "[0-9]+" :: (Int, Int)