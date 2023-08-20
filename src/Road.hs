module Road (minRoad) where
import Data.List

toTuple :: Num a => [a] -> [(a, a, a)]
toTuple [] = []
toTuple [a] = [(a, 0, 0)]
toTuple [a, b] = [(a, b, 0)]
toTuple (a : b : c : xs) = (a, b, c) : toTuple xs

minRoad :: (Num a, Ord a) => (a, a, [a], [a]) -> [(a, a, a)] -> (a, a, [a], [a])
minRoad (a0, b0, accA, accB) [] = (a0, b0, accA, accB)
minRoad (a0, b0, accA, accB) ((a, b, c) : xs) = minRoad (cal (a0, b0, a, b, c, accA, accB)) xs
  where
    a0a = a0 + a
    b0bc = b0 + b + c
    b0b = b0 + b
    a0ac = a0 + a + c
    cal (a0, b0, a, b, c, accA, accB)
      | a0a > b0bc && b0b > a0ac = (b0bc, a0ac, accB ++ [b, c], accA ++ [a, c])
      | a0a > b0bc && b0b <= a0ac = (b0bc, b0b, accB ++ [b, c], accB ++ [b])
      | a0a <= b0bc && b0b <= a0ac = (a0a, b0b, accA ++ [a], accB ++ [b])
      | otherwise = (a0a, a0ac, accA ++ [a], accA ++ [a, c])

run filePath = do
  contents <- readFile filePath
  let list = toTuple $ map read $ lines contents
   in do
        print list
        return $ minRoad (0, 0, [], []) list

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show, Read)

type RoadSystem = [Section]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int, Int)]

trd3 (_, _, s) = s

fst3 (a, _, _) = a

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = if null pathA then 0 else trd3 $ head pathA
      priceB = if null pathB then 0 else trd3 $ head pathB
      a2a = priceA + a
      b2b = priceB + b
      b2b2a = b2b + c
      a2a2b = a2a + c
      newPathA = if a2a < b2b2a then (A, a, a2a) : pathA else (C, c, b2b2a) : (B, b, b2b) : pathB
      newPathB = if b2b < a2a2b then (B, b, b2b) : pathB else (C, c, a2a2b) : (A, a, a2a) : pathA
   in (newPathA, newPathB)

optimiRoad :: RoadSystem -> Path
optimiRoad [] = []
optimiRoad x =
  let (pathA, pathB) = foldl roadStep ([], []) x
   in if trd3 (head pathA) > trd3 (head pathB)
        then pathA
        else pathB

toRoadSystem :: [Int] -> RoadSystem
toRoadSystem [] = []
toRoadSystem [a] = [Section a 0 0]
toRoadSystem [a, b] = [Section a b 0]
toRoadSystem (a : b : c : xs) = Section a b c : toRoadSystem xs

run2 filePath = do
  contents <- readFile filePath
  let list = map read $ lines contents
      roadSystem = toRoadSystem list
      bestPath = optimiRoad roadSystem
      costTime = trd3 $ head bestPath
   in do
        print roadSystem
        print $ reverse bestPath
        print $ intercalate "->" $ map (show . fst3) $ reverse bestPath
        print costTime