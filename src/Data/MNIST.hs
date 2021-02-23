{-# LANGUAGE OverloadedStrings #-}
module Data.MNIST
    ( readLabels
    , readImages
    , showImage
    , dataToSample
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits (shiftL, (.|.))
import Data.Foldable
import Data.Char (ord)

readInteger :: ByteString -> Maybe (Int, ByteString)
readInteger "" = Nothing
readInteger bs = Just (BS.foldl' toInt 0 $ BS.take 4 bs, BS.drop 4 bs)
    where
        toInt acc w = shiftL acc 8 .|. fromIntegral w

readBytes :: ByteString -> [Int]
readBytes = BS.foldr' (\w acc -> fromIntegral w : acc) []

readLabels :: ByteString -> Maybe [Int]
readLabels bs = do
    (magic, bs1) <- readInteger bs
    if magic == 2049
        then do
            (numOfLabels, bs2) <- readInteger bs1
            return $ readBytes bs2
        else Nothing

readImages :: ByteString -> Maybe [[Int]]
readImages bs = do
    (magic, bs1) <- readInteger bs
    if magic == 2051
        then do
            (numOfImages, bs2) <- readInteger bs1
            (numOfRows, bs3) <- readInteger bs2
            (numOfCols, bs4) <- readInteger bs3
            let all = readBytes bs4
            return $ split (numOfRows * numOfCols) all
        else Nothing

showImage :: [Int] -> String
showImage = foldr' (\row acc -> showRow row ++ "\n" ++ acc) "" . split 28
    where
        showRow = map (showPixel . ord) . concat . map show
        showPixel n
            | n < 50 = ' '
            | n < 150 = '-'
            | n < 200 = '*'
            | otherwise = '&'

split :: Int -> [a] -> [[a]]
split _ [] = []
split k xs = take k xs : split k (drop k xs)

dataToSample :: [[Int]] -> [Int] -> [([Double], [Double])]
dataToSample imgs labels = zipWith (,) imgs' labels'
    where
        imgs' = (map . map) fromIntegral imgs
        labels' = map (labelToLayer) labels
        labelToLayer x = update x 1.0 $ replicate 10 0.0 

update :: Int -> a -> [a] -> [a]
update i k xs = go 0 xs
    where
        go _ [] = []
        go n (x:xs)
            | n == i = k : xs
            | otherwise = x : go (n + 1) xs
