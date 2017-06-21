module ANN where

import System.Random
import Control.Monad

import Types
import Backprop

errorTotal :: [Double] -> [Double] -> Double
errorTotal xs ys = (1/2) * (sum . map (**2) $ zipWith (-) xs ys)

createNet :: [Int] -> IO Network
createNet xs = create (head xs) (tail xs)
    where
        create _ [] = return []
        create p (x:xs) = do
            ns <- replicateM x (neuron p)
            fmap (ns :) (create x xs)
        neuron p = do
            ws <- replicateM (p + 1) (randomRIO (-1, 1))
            return (Neuron ws 0 0)

runNet :: [Double] -> Network -> [Double]
runNet xs net = map (\(Neuron _ o _) -> o) . last $ feedForward xs net

backprop :: [Double] -> [Double] -> Network -> Network
backprop xs ts net =
    let net' = calcHidError . calcOutError ts $ feedForward xs net
    in backpropError xs net'

overallError :: [[Double]] -> [[Double]] -> Network -> Double
overallError xss yss net =
    sum $ zipWith (\xs ys -> errorTotal (runNet xs net) ys) xss yss

train :: Int -> Double -> [[Double]] -> [[Double]] -> Network -> Network
train max minErr xss yss net = run 0 net
    where
        run k net
            | k == max = net
            | err net < minErr = net
            | otherwise = run (k + 1) $ epoch xss yss net
        err = overallError xss yss

train' :: Int -> Double -> [[Double]] -> [[Double]] -> Network -> [Double]
train' max minErr xss yss net = run 0 net
    where
        run k net
            | k == max = []
            | err net < minErr = []
            | otherwise = err net : (run (k + 1) $ epoch xss yss net)
        err = overallError xss yss

epoch :: [[Double]] -> [[Double]] -> Network -> Network
epoch [] [] net = net
epoch (xs:xss) (ys:yss) net =
    let net' = backprop xs ys net
    in epoch xss yss net'

-- Learn the XOR-operator

samples = [[0, 0], [0],
           [0, 1], [1],
           [1, 0], [1],
           [1, 1], [0]]

xs = filter (\xs -> length xs == 2) samples
ys = filter (\xs -> length xs == 1) samples

test :: IO Network
test = do
    net <- createNet [2,2,1]
    let net' = train 10000 0.1 xs ys net
    return net'

someFunc :: IO ()
someFunc = putStrLn "someFunc"
