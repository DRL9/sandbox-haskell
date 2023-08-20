module Basic where

-- 数学运算
math1 = [1 + 1, 1 - 1, 4.2 * 1.2, 1.3 / 1.9]

-- 取模
math2 = 5 `mod` 2

-- 逻辑运算
logic1 = [1 == 1, 1 /= 2, [1, 2] == [1, 2], [1, 2, 3] /= [1, 2], "abc" == "abc"]

-- bool 运算
bool1 = [True && True, True || False, not False]

-- 条件表达式, 必须有 else
if1 a = if a > 1 then 2 else 3

-- 函数声明
-- 无参数
func1 = [1, 2]

-- 有参数
func2 a b = [a, b, 1]

-- 列表
-- 连接列表
list1 a b = a ++ b

-- 连接字符串. 字符串是特殊的列表 . 当在大列表中使用 ++ 会很耗时
list2 = "hello " ++ "world"

list3 = ['h', 'e'] ++ "llo"

-- 使用 : 向 head 插入列表
list4 = 4 : [1, 2, 3]

-- 取元素
list5 = [1, 2, 3] !! 0

list6 = "abc" !! 1

-- 比较, 从第一个开始比较， 如果相等，则比较第二个， 返回第一个不相等的比较结果
list7 = [1, 2, 3] < [2, 3, 4]

list8 = [1, 3, 4] < [1, 6, 1]

-- 取第一个元素
list9 = head [1, 2, 3]

-- 取除了第一个元素
list10 = tail [1, 2, 3]

-- 取最后一个元素
list11 = last [1, 2, 3]

-- 取除了最后一个元素
list12 = init [1, 2, 3]

-- 取长度
list13 = length [1, 2, 3, 4]

-- 判断是否是空列表
list14 = null []

-- 逆序列表
list15 = reverse [1, 2, 3, 4, 6]

-- 取前 n 个
list16 = take 3 [1, 2, 3, 4, 9]

-- 去掉前 n 个
list17 = drop 2 [1, 2, 3, 4, 5, 6]

-- 取最大， 最小
list18 = [maximum [1, 2, 3], minimum [1, 2, 3]]

-- 求和
list19 = sum [1, 2, 3, 4]

-- 求乘积
list20 = product [2, 3, 4]

-- 是否包含
list21 = 12 `elem` [2, 3, 4, 5, 12]

-- 范围
-- step = 1
range1 = [1 .. 10]

-- step = 3 (4-1)
range2 = [1, 4 .. 10]

--  字母
range3 = ['a', 'c' .. 'z']

-- 循环列表, [1,2,3,1,2,3,...]
list22 = take 10 (cycle [1, 2, 3])

-- 重复列表 [1,1,1]
list23 = replicate 3 1

list24 = take 3 (repeat 1)

-- 列表解析式

-- | 前面的用来计算元素， | 后面的表示 所属的集合， ',' 后面的是 filter
comprehension1 = [x | x <- [1, 2, 3], x > 1]

comprehension2 = [x * 2 | x <- take 10 [1, 3 .. 31], odd x, x * 2 > 10]

-- n*m 个组合进行过滤
comprehension3 = [[x, y] | x <- take 10 [1 .. 44], y <- take 3 [2, 5 .. 100], x * y <= 30]
