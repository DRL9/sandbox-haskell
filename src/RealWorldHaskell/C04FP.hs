module RealWorldHaskell.C04FP (handleFile) where

import System.Environment (getArgs)
import Data.List
import Data.Char
import Data.Bits

interactWith fn inFile outFile = do
        inContent <- readFile inFile
        writeFile outFile (fn inContent)



handleFile = let doWith fn = do
                  args <- getArgs
                  case args of
                    [inFile, outFile] -> interactWith fn inFile outFile
                    _ -> putStrLn "error: need 2 argument exactly"
        in doWith id


-- break 接受一个测试函数和 list, 将该 list 分割成一个 list 的二元组, 预测成功中断
-- 将内容分割成 行， 兼容 \r\n 和 \n
splitLines [] = []
splitLines str = let isLineTerminator c = c == '\n' || c == '\r'
                     (pre, rest) = break isLineTerminator str
                in pre : case rest of
                                ('\r':'\n':xs) ->  splitLines xs
                                ('\r':xs) ->  splitLines xs
                                ('\n':xs) ->  splitLines xs
                                _ -> []

-- infix 中缀函数 加上 ` `, 参数有两个
plus a b = a + b
test1 = 1 `plus` 2
test2 = plus 1 2
test3 = (+) 1 2

-- Data.List
-- 空
t1 = null []
-- 去掉最后一个元素
t2 = init [1,2,3] -- [1,2]
-- list 没有储存 length ，每次调用 length 都会实时遍历， 遇到无穷数组时会卡住，所以不要随便用 length
t3 = length []
t4 = length [1, 2]
-- 平整list flat
t5 = concat [[1], [2,3]]
-- bool
t6 = and [True, False]
t7 = or [True, False]
-- every, some
t8 = all odd [1,3,5]
t9 = any odd [2,3,4]
-- 获取前面的
t10 = take 10 [1..20]
-- 去掉前面的，获取后面的
t11 = drop 3 [1..20]
-- 按位置分割成元组
t12 = splitAt 3 [1..20]
-- 获取开头的满足 predicate 的 list
t13 = takeWhile odd [1,3,4,5,7] -- [1,3]
-- 删除开头的满足 predicate 后， 剩余的 list
t14 = dropWhile even [2,4,5,6,8] -- [5,6,8]
-- span 与 break 相反, 预测失败了中断
t15 = span odd [1,3,4,5] -- ([1,3], [4,5])
-- 搜索 list
t16 = 2 `elem` [1..3]
t17 = filter odd [1..20]
t18 = isPrefixOf [1,2] [1..10]
t19 = isInfixOf [4,5] [1..10]
t20 = isSuffixOf [6..10] [1..10]

-- zip , 取最短的 , 还有 zip3 zip4 .. zip7
t21= zip [1,3..9] ['a','b'..'z']
t22 = zipWith (-) [10,9..1] [1..10]
zip' = zipWith (\a b->(a,b)) -- 等效 zip
t21_1 = zip' [1,3..9] ['a','b'..'z']

-- 字符串处理
-- 行处理
l1 = lines "this is a line.\n second line"
l2 = unlines ["line1", "line2"]
-- 单词处理
w1 = words "this is a word."
w2 = unwords ["this", "is", "a", "word"]

-- Exercises
-- e1
safeHead [] = Nothing
safeHead a = Just (head a)
safeTail [] = []
safeTail a = tail a
safeLast [] = Nothing
safeLast a = Just (last a)
safeInit [] = []
safeInit a = init a

-- e2
splitWith' fn [] = []
splitWith' fn a = let   bar [] l1 l2 = l2 ++ [l1]
                        bar (x:xs) l1 l2 = if fn x then bar xs (l1++[x]) l2
                                        else bar xs [] (l2 ++ [l1])
                        in bar a [] []

-- e3 打印文件每行的第一个单词 todo

-- e4 转置文本文件 todo
-- transposeFile [] = []
-- transposeFile a = let   bar [] l1 l2= l1 l2
--                         bar (x:xs) l1 l2= bar xs (l1 ++ take 1 x) (l2 ++ tail x)
--                 in bar a [] []

-- handleTransposeFile = do
--                         args <- getArgs
--                         content <- case args of
--                                         [a] -> readFile a
--                                         _   -> error "need 1 arg"
--                         putStrLn (transposeFile (lines content))


-- 使用递归替代循环
-- haskell 实现了尾部调用优化(TCO)， 所以不管递归多少次，空间复杂度是常数
asInt :: String -> Int
asInt [] = 0
asInt str = let loop acc [] = acc
                loop acc (x:xs) = loop (acc * 10 + digitToInt x) xs
            in loop 0 str

square' [] = []
square' (x:xs) = x*x : xs

toUpperCase = map toUpper
-- 自定义 map
map' f (x:xs) = f x : map' f xs
map' _ _ = []

-- filter
oddList (x:xs) | odd x = x : oddList xs
               | otherwise = oddList xs
oddList _ = []

oddList' = filter odd

-- reduce
sum' xs = foo 0 xs
        where foo acc (x:xs) = foo (acc+x) xs
              foo acc _ = acc

-- 左边开始
-- foldl 的 thunk 会占用内存空间，导致性能问题， 使用 Data.List.foldl' 替代
sum'' xs = foldl (+) 0 xs
-- 右边开始
sum''' xs = foldr (+) 0 xs

-- 位运算
-- 左移
b1 :: Int
b1 = shiftL 1 1

-- 与
b2 :: Int
b2 = 1 .&. 1
-- 或
b3 :: Int
b3 = 1 .|. 0

-- Exercise
asInt_fold ('-':xs) = -1 * asInt_fold xs
asInt_fold xs = foldl' step 0 xs
        where step acc cur = acc * 10 + (digitToInt cur)

asInt_either ('-':xs) = let a = asInt_either xs
                        in case a of
                                Left _ -> a
                                Right x -> Right (-1 * x)
asInt_either xs = foldl' step (Right 0) xs
        where step acc cur = case acc of
                                Left _ -> acc
                                Right x  | isDigit cur -> Right (x * 10 + (digitToInt cur))
                                         | otherwise -> Left ("non-digit '" ++ [cur] ++ "'")

-- 匿名函数
-- \ 开头 , -> 左边参数，右边函数体
lamb1 = map (\ a -> a+1) [1..10]

-- Partial function application 柯里化
-- 对于中缀函数， 柯里化可以判断缺的是哪个参数
x1 = (2^) 3
x2 = (^2) 3

-- as-pattern , @ 将左边解构成右边的
suffixes all@(_:xs) = all : suffixes xs
suffixes _ = []

-- 组合
compose f g x = f (g x)
suffixes1 = compose init tails
suffixes2 = init . tails

-- seq 强制计算结果， 可以不产生 thunk
strictList (x:xs) = x `seq` x : strictList xs
strictList [] =[]