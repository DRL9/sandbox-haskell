module Cookbook.Collection where

-- 如何生成范围内的列表
list_step_1 = [1..10]
list_step_2 = [1,3..10]  -- 不会超过10

-- 如何对列表求和
sum_1 = sum [1,2,3,4]

-- 如何对列表求积
product_1 = product [2,3,4]