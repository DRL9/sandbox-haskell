module RealWorldHaskell.C09BetterPredicate (betterFind) where


import Control.Exception (handle, SomeException(..), bracket)
import System.IO (openFile, IOMode(..), hFileSize, hClose)
import System.Directory (
    Permissions,
    getModificationTime,
    getPermissions,
    getDirectoryContents,
    doesDirectoryExist,
    )
import System.FilePath (
    (</>),
    )
import Control.Monad (forM, filterM)
import Data.Time.Clock (UTCTime)

saferFileSize path = handle nothingErr $
    -- bracket 类似于 finally
    bracket (openFile path ReadMode) hClose $ \file -> do
                                        size <- hFileSize file
                                        return $ Just size

nothingErr :: SomeException -> IO (Maybe a)
nothingErr a = return Nothing


betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p f = getRecursiveContents f >>= filterM check
    where check f = do
            permission <- getPermissions f
            size <- saferFileSize f
            mtime <- getModificationTime f
            return $ p f permission size mtime



type Predicate = FilePath        -- path to directory entry
                -> Permissions     -- permissions
                -> Maybe Integer   -- file size (Nothing if not file)
                -> UTCTime
                -> Bool

-- 预测函数生产函数
type InfoP a =  FilePath        -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> UTCTime
             -> a

pathP :: InfoP FilePath
pathP p _ _ _ = p

permP :: InfoP Permissions
permP _ p _ _ = p

sizeP :: InfoP Integer
sizeP _ _ Nothing _ = -1
sizeP _ _ (Just a) _ = a

mtimeP :: InfoP UTCTime
mtimeP _ _ _ t = t


liftP :: (a->b->c) -> InfoP a -> b -> InfoP c
liftP p f b a1 a2 a3 a4 = f a1 a2 a3 a4 `p` b

liftP2 :: (a->b->c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f1 f2 a1 a2 a3 a4 = f1 a1 a2 a3 a4 `q` f2 a1 a2 a3 a4

greaterP, lesserP, (>?) :: Ord a => InfoP a -> a -> Predicate
greaterP = liftP (>)
lesserP = liftP (<)

equalP,(==?) :: Eq a => InfoP a -> a -> Predicate
equalP = liftP (==)

andP,orP :: Predicate -> Predicate -> Predicate
andP = liftP2 (&&)
orP = liftP2 (||)
(&&?) = andP
(==?) = equalP
(>?) = greaterP

-- 设置运算符优先级, 数字越大，优先级越高
infix 4 ==?
infix 4 >?
infix 3 &&?



liftPath :: (FilePath -> a) -> InfoP a
liftPath f a _ _ _ = f a


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents p = do
    names <- getDirectoryContents p
    let files = filter (\f -> f `notElem` [".", ".."]) names
    all <- forM files $ \f -> do
        let path = p </> f
        isDirectory <- doesDirectoryExist path
        if isDirectory then getRecursiveContents path
        else return [path]
    return $ concat all