module RealWorldHaskell.C01GetStarted where

import Data.Ratio

-- list 必须是相同的数据类型
l1= [1,2,3]
l2 = ["a","b"]

-- 范围 list
r1= [1..10]
r2= [1.0 , 1.2 .. 2.0]
-- 无穷
r3= [1,3..]

-- 连接 list
l3= [1,2]++[4,5]
--  : 的第一个必须是 element
l4 = 1:[1,2]

-- 字符串就是字符list
b1= ['a','b'] == "ab"
s1= 'a' :"cde"

-- 整数精度可以无限大
i1= 2^1000
-- 进行分数计算 需要 import Data.Ratio
ra1= 10%32 + 20%34