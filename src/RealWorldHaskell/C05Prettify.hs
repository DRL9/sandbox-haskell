module RealWorldHaskell.C05Prettify
    (
        Doc,
        empty,
        line,
        char,
        double,
        text,
        string,
        series,
        pretty,
        compact,
        (+++)
    )
    where


import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)


data Doc = Empty
         | Line
         | Char Char
         | Text String
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Eq, Show)

-- 提供工厂函数， 防止暴露构造器
empty = Empty
line = Line

char :: Char -> Doc
char = Char

double :: Double -> Doc
double a = text (show a)

text :: String -> Doc
text = Text

string :: String -> Doc
string = enclose '"' '"'. hcat . map oneChar

-- 用来转义
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           +++ text (replicate (4 - length h) '0')
           +++ text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) +++ smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff


enclose :: Char -> Char -> Doc -> Doc
enclose c1 c2 doc = char c1 +++ doc +++ char c2

-- 多个合并成一个
hcat :: [Doc] -> Doc
hcat = foldr (+++) Empty

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other


(</>) :: Doc -> Doc -> Doc
x </> y = x +++ softline +++ y

-- 多个合并成一个, 添加换行
fsep :: [Doc] -> Doc
fsep = foldr (</>) Empty


(+++) :: Doc -> Doc -> Doc
Empty +++ a = a
a +++ Empty = a
x +++ y = Concat x y

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [x] = [x]
punctuate p (x:xs) = (x +++ p) : punctuate p xs


compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> [Char]
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs


series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
