module RealWorldHaskell.C07IO
    (test1)
    where


import System.Directory (getDirectoryContents, renameFile, removeFile)

import Data.Char (toUpper)
import System.IO (
    hPutStrLn,
    hGetLine,
    hIsEOF,
    hSetBuffering,
    hFlush,
    BufferMode(..),
    Handle,
    openFile,
    hClose,
    IOMode(..)
    )

import System.Environment (getArgs, getEnvironment)

nameStr name = "Welcome!\n"
    ++ "Your name is " ++ name

-- <- 取 IO (t) 的值
-- do 中， let 不需要 in
test1 = do
    putStrLn "What's your name?"
    line <- getLine
    let outStr = nameStr line
    putStrLn $ outStr


-- System.IO
-- h* 方法是操作文件的
-- return 把一个值包裹在monad中， 比如 IO()

upperContent :: Handle -> Handle -> IO()
upperContent inH outH = do
       eof <- hIsEOF inH
       if eof
            then return ()
            else do
                inStr <- hGetLine inH
                hPutStrLn outH $ map toUpper inStr
                upperContent inH outH


test2 = do
    inH <- openFile "./app/Main.hs" ReadMode
    outH <- openFile "./tmp/aaa" WriteMode
    upperContent inH outH
    hClose inH
    hClose outH

-- 对于 Handle， 操作系统会记录上次读取的位置
-- 使用 hTell 获取该位置
-- 使用 hSeek 重置该位置

-- System.Directory
-- 文件操作， renameFile, removeFile
testD1 = getDirectoryContents "."
testD2 = renameFile "./tmp/aaa" "./tmp/ccc"

-- hGetContent 是 lazy , 所以获取文件内容不会造成内存泄漏
-- readFile 用的就是 hGetContent
-- writeFile 也是 lazy
-- readFile --> writeFile 实际上类似 pipe 执行， 不会占用太大内存

-- interact fn , stdin 到 stdout 的转换


-- IO (), IO String 这些叫做 Action


str2action :: String -> IO ()
str2action a = putStrLn a

-- do 的每条语句生成一个 I/O action, 除了 let
-- mapM_ 将list 转成 action list , 返回 Monad ()
-- mapM 会保留 每个action 返回的结果, 返回 Monad []
-- *_ 尾缀加 _ 一般表示丢弃返回结果
test3 = do
    mapM_ (str2action . show) [1..10]
    mapM (str2action . show) [33..36]

-- >> 按顺序执行 action , 丢弃前面的返回结果
-- >>= 将前面的返回结果，作为下一个的入参
test4 = putStr "hello" >> putStr " world" >> putStrLn "!"
test5 = getLine >>= foo "your input is "
    where foo str1 str2 = putStrLn $ str1 ++ str2

-- 读写文件，都会先放到缓存区中
-- 有3种模式， NoBuffering, LineBuffering, and BlockBuffering
-- 可以 flush buffer 到文件

test6 = do
    outH <- openFile "./tmp/aac" WriteMode
    hSetBuffering outH $ BlockBuffering $ Just 1024
    hPutStrLn outH "hello"
    hFlush outH
    hPutStrLn outH "world"
    hClose outH
    return ()

-- 命令行参数
test7 = do
    args <- getArgs
    mapM putStrLn args

-- 环境变量
test8 = do
    envs <- getEnvironment
    mapM_ (putStrLn . fst) envs