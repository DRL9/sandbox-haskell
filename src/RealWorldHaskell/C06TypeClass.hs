{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module RealWorldHaskell.C06TypeClass where



import Data.List (concat, intersperse)

-- 定义 isEqual 方法
class BasicEq a where
    isEqual :: a -> a -> Bool

    isNotEqual :: a -> a-> Bool
    -- 默认实现
    isNotEqual a b = not (isEqual a b)

-- isEqual 对指定类型的具体实现
instance BasicEq Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _ = False

-- 对于类型别名， instance 默认时不支持的， 需要在文件头部加上  {-# LANGUAGE TypeSynonymInstances #-}
type Int' = Int
instance BasicEq Int' where
    isEqual 1 1 = True
    isEqual _ _ = False


-- 类似标准库中的 Eq
class Eq' a where
    (===),(/==) :: a -> a -> Bool
    -- 实例只需要实现其中一个
    a === b = not (a /== b)
    a /== b = not (a === b)

-- 内置的 typeclass

data Color = Red | Green | Blue
-- Show
instance Show Color where
    show Red = "red"
    show Green = "green"
    show Blue = "blue"

a = Red
a1 = show a

-- Read
b = read "10" :: Double

-- 数字类型
-- https://book.realworldhaskell.org/read/using-typeclasses.html#numerictypes.summary

-- Ord 可排序

-- Either 处理可出现的异常
boo a | length a > 0 = Right a
      | otherwise = Left "to small"


-- 在头部加上 OverlappingInstances, 让编译器选择更具体的实现， 而不是抛出异常
-- [a] 和 String 重叠了
class Foo a where
    foo :: a -> String

instance Foo a => Foo [a] where
    foo = concat . intersperse ", " . map foo

instance Foo Char where
    foo c = [c]

instance Foo String where
    foo = id

-- newtype 重命名已有类型
-- 只能有一个构造器
-- 构造器只能有一个参数
-- 跟 type 相比， 多一个构造器来约束
type Pos = (Int, Int)
newtype Position = Position (Int, Int)
    deriving (Show)

