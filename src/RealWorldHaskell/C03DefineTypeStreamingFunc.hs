module RealWorldHaskell.C03DefineTypeStreamingFunc where
import Data.List

-- BookInfo 是类型名，  Book 是构造器
data BookInfo  = Book Int String [String]
    deriving (Show) -- 加上这个让 repl 可以打印该类型的值，而不是报错


book1 = Book 1 "The Art of Computer" ["Knuth"]

-- 类型别名
type CustomerID = Int

-- 类型名和构造器可以同名
data BookReview = BookReview BookInfo Int
    deriving (Show)

bookR1 = BookReview book1 001

--  多个构造器
data Bool' = True' | False'

type CardNumber = String
data BillingInfo = CashOnDelivery
    | CreditedCard CardNumber
    deriving (Show)

bill1 = CashOnDelivery
bill2 = CreditedCard "No001"

-- 模拟枚举
data Color = Red
    | Blue
    | Green
    | Black
    | Mix String String
    deriving (Eq, Show)

-- 模式匹配, 从上到下
showColorHex Red = "#ff0000"
showColorHex Black = "#000000"
showColorHex (Mix c1 c2) =  c1 ++ "," ++ c2
showColorHex _ = "other"
s1= showColorHex Red
s2= showColorHex Green
s3 = showColorHex (Mix "#000" "#f00")
--
sumList (x:xs) = x + sumList xs
sumList [] = 0
sum1 = sumList [1..100]

-- 使用模式匹配获取字段
bookTitle (Book _ title _) = title
b2 = Book 001 "The Art of War" ["Mark"]
t1 = bookTitle b2

-- record 语法 作为 属性访问器的语法糖
data Customer = Customer {
    customerId :: CustomerID,
    customerAddress :: [String]
} deriving (Show)
c1 = Customer 333 ["shenzhen"]
c2 = Customer {
    customerId = 34,
    customerAddress = ["guangdong"]
}
ad1 = customerAddress c1
ad2 = customerId c2

-- 带参数的类型
data Maybe' a = Just' a
    | Nothing'
    deriving (Eq, Show)

m1 = Just' 12
m2 = Nothing'
m12 = m1 /= m2

-- 递归的类型定义
data Tree a = Node a (Tree a) (Tree a)
    | Empty
    deriving (Show)

tree1 = Node 1 (Node 2 Empty Empty) Empty

data Tree2 a = Node2 a (Maybe (Tree2 a)) (Maybe (Tree2 a))
    deriving (Show)


tree2 = Node2 (Just 1) Nothing (Just (Node2  (Just 55) Nothing Nothing))


-- 抛出异常
srd2 a = if null (tail a)
    then error "too short"
    else head (tail a)


-- 解构数组
safeSrd (_:a:_)=Just a;
safeSrd _ = Nothing

-- 局部变量 let .. in .. , 返回 in 后面表达式的结果
-- = 右边绑定的是表达式， 并不会马上计算，而是要等到需要使用的时候才计算
-- 注意缩进， let 绑定的变量缩进要对齐

lend amount balance = let reserve   = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance
-- 嵌套 let 和 shadow
foo = let x = 1
        in ((let x = "2" in x), x)

-- where
lend3 amount balance = if balance < reserve
                        then Nothing
                        else Just newBalance
    where reserve = 100
          newBalance = balance - amount

-- case .. of .. 作为模式匹配
bar a = case a of
        Nothing -> "none"
        Just "1" -> "one"
        Just a -> a


--  在函数中使用 | 作为 guard. | {condition} = {expr}
drop2 n xs | n <=0 = xs
drop2 _ [] = []
drop2 n (_:xs) = drop2 (n-1) xs

lend4 amount balance | amount > balance = Nothing
                     | newBalance < reserve = Nothing
                    | otherwise = Just newBalance
    where reserve =100
          newBalance = balance- amount

-- exercises
length' [] = 0
length' (_:xs) = 1 + length' xs

mean a = (sum a) / fromIntegral (length a)

palindrome a = a ++ reverse' a
    where reverse' [] = []
          reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)


sortListByLength :: [[a]] -> [[a]]
sortListByLength = sortBy (\ a b -> compare (length a) (length b))

intersperse' :: a -> [[a]] -> [a]
intersperse' s [] = []
intersperse' s [a] = a
intersperse' s (x:xs) = x ++ [s] ++ (intersperse' s xs)

-- 二叉树高度
treeHeight :: (Num b, Ord b) => Tree a -> b
-- treeHeight a = case a of
--                 Empty -> 0
--                 Node _ Empty Empty -> 1
--                 Node _ a b -> max (1 + treeHeight a) (1 + treeHeight b)
treeHeight Empty = 0
treeHeight (Node _ Empty Empty) = 1
treeHeight (Node _ a b)= max (1 + treeHeight a) (1 + treeHeight b)

--
data Direction = Left' | Right' | Straight'
    deriving (Show)
data Point = Point {pointX :: Double, pointY :: Double}
getDirection :: Point -> Point -> Point -> Direction
getDirection a b c = let getRadian a b = atan (((pointY b) - (pointY a)) / ((pointX b) - (pointX a)))
                         toAngle a = a * 180 / pi
                         getAngle a b = toAngle (getRadian a b)
                         angle = (getAngle a b) - (getAngle c b)
                         judge angle | angle > 90 = Right'
                                      | angle == 180 = Straight'
                                      | angle == 0 = Straight'
                                      | otherwise = Left'
    in judge angle