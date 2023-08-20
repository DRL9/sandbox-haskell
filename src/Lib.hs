module Lib
  ( helloFunc,
    Shape (Circle),
    Point,
  )
where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as S
import Data.Char
import Data.List
import qualified Data.Map as M
-- import Data.Semigroup (Option (Option))
import Geometry (sphereVolume)
import Geometry.Cube (cubiodVolume)

helloFunc :: IO ()
helloFunc =
  forever $ do
    putStr "give me input: "
    l <- getLine
    putStrLn $ map toUpper l

hello2 :: IO ()
hello2 = do
  putStrLn "input sentences"
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseLine line
      hello2

reverseLine :: String -> String
reverseLine = unwords . map reverse . words

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

encode :: Int -> String -> String
encode shift = map (chr . (+) shift . ord)

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (y2 - y1) * (x2 - x1)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Show, Read)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)

lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp lockerNumber map =
  case M.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " is not exist"
    Just (state, code) ->
      if state == Taken
        then Left $ "Locker number" ++ show lockerNumber ++ " is already taken"
        else Right code

lockers :: LockerMap
lockers =
  M.fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "JAH3I")),
      (103, (Free, "IQSA9")),
      (105, (Free, "QOTSA")),
      (109, (Taken, "893JJ")),
      (110, (Taken, "99292"))
    ]

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeSingleton x = Node x EmptyTree EmptyTree

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree a EmptyTree = treeSingleton a
insertTree a (Node x left right)
  | a == x = Node a left right
  | a < x = Node a (insertTree x left) right
  | otherwise = Node x left (insertTree a right)

solveRPN :: (RealFloat a, Read a) => String -> a
solveRPN =
  head . solve . words
  where
    solve = foldl foldFn []
    foldFn (x : y : xs) "+" = (x + y) : xs
    foldFn (x : y : xs) "-" = (y - x) : xs
    foldFn (x : y : xs) "*" = (x * y) : xs
    foldFn (x : y : xs) "/" = (y / x) : xs
    foldFn acc item = read item : acc

run1 = fmap (+ 1) $ Right 2

run2 = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said" ++ line ++ " backwards!"

run3 = [(* 3), (* 4)] <*> [1, 2]

run = fmap (\x -> 2 - x * 2) (Just 23)

run' = fmap (\x -> 2 - x * 2) [1, 2, 3]
