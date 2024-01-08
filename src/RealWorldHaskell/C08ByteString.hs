module RealWorldHaskell.C08ByteString where


-- String 性能不行
-- ByteString 提供高性能字符串处理

-- 确定是否是可执行文件

-- 数据作为 Word8 处理
import qualified Data.ByteString.Lazy as L

-- 数据作为 Char8 处理, 只能处理 ascii
import qualified Data.ByteString.Lazy.Char8 as LC



-- Data.ByteString.Lazy 读取成二进制字节
-- Word8 一个字节
hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elf
    where elf = L.pack [0x7f, 0x45, 0x4c, 0x46]


isElfFILE :: FilePath -> IO Bool
isElfFILE path = do
    content <- L.readFile path
    return (hasElfMagic content)


-- Data.ByteString.Lazy.Char8 是读取成字符
-- String 转 ByteString
test1 = LC.pack "hello"

-- 读取 csv ，获取 Close 列的最大值
highestClose :: IO (Maybe Int)
highestClose = do
    content <- LC.readFile "./resources/prices.csv"
    return (maximum $ (Nothing:) $ close content)

close = map readPrice . map (!!4) . map (LC.split ',')  . LC.lines
readPrice :: LC.ByteString -> Maybe Int
readPrice str = case LC.readInt str of
            Nothing -> Nothing
            Just (yuan, rest) -> case LC.readInt $ LC.tail rest of
                                Just (fen, _) -> Just $ yuan * 100 + fen
                                Nothing -> Nothing

