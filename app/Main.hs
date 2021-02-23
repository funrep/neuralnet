module Main where

import System.Environment (getArgs)
import qualified Data.ByteString as BS

import Data.MNIST

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> printImages fp
        _ -> putStrLn "Supply filepath to images"

printImages :: FilePath -> IO ()
printImages fp = do
    mImgs <- getImages fp
    case mImgs of
        Just imgs -> mapM_ (putStrLn . showImage) $ take 10 imgs
        _ -> return ()

getLabels :: FilePath -> IO (Maybe [Int])
getLabels fp = do
    content <- BS.readFile fp
    return $ readLabels content

getImages :: FilePath -> IO (Maybe [[Int]])
getImages fp = do
    content <- BS.readFile fp
    return $ readImages content
