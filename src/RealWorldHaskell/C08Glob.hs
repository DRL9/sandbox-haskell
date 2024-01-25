module RealWorldHaskell.C08Glob (namesMatching) where


import System.Directory (
    doesFileExist,
    doesDirectoryExist,
    getCurrentDirectory,
    getDirectoryContents
    )
import System.FilePath (
    FilePath,
    splitFileName,
    dropTrailingPathSeparator,
    (</>)
    )

import Control.Monad (forM)
import Control.Exception (handle)
import Control.Exception.Base (Exception(..),SomeException(..))

import RealWorldHaskell.C08GlobRegex (matchesGlob)


namesMatching :: FilePath -> IO [FilePath]
namesMatching pat | isPattern pat = do
                        (dirname, basePat) <- case splitFileName pat of
                                     ("", f) -> do
                                        cur <- getCurrentDirectory
                                        return (cur, f)
                                     a -> return a
                        dirnames <- if isPattern dirname
                                        then namesMatching $ dropTrailingPathSeparator dirname
                                    else return [dropTrailingPathSeparator dirname]
                        results <- forM dirnames $ \dir -> ls dir basePat
                        return $ flatten results
                  | otherwise = do
                        exists <- doesNameExist pat
                        return (if exists then [pat]
                                else [])

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
                     fileExist <- doesFileExist name
                     if fileExist then return True
                     else doesDirectoryExist name

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

isHidden ('.':_) = True
isHidden _       = False


ls :: FilePath -> String -> IO [FilePath]
ls dir pat = do
    files <- getDirectoryContents dir
    -- 没有异常处理 matchesGlob 里面有 error
    let matches = filter (\f -> f `matchesGlob` pat && (not . isHidden) f) files
    return $ map (dir </>) matches

