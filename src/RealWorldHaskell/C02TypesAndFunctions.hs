module RealWorldHaskell.C02TypesAndFunctions where
-- Int 32-bit or 64-bit
i0=  2::Int
-- Integer 没有边界
i1 =2^100

-- list
l0 = drop 1 [1..5]

--
a =take 1  [2,3..7]

-- tuple
t0= fst (1,2)

--
drop' n xs = if null xs || n <=0
    then xs
    else
        drop' (n-1) (tail xs)

-- lazy evaluation
or' a b = if a then a else b
-- 传递的表达式参数不会马上运算。
or1 = or' ((length (take 10 [1..])) > 3) ((( length (take 12 [2..])) >1))

--  thunk: 延时计算的表达式


--
lastButOne:: [a]->a
lastButOne xs  = head (drop ((length xs)- 2) xs)
