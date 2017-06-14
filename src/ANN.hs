module ANN where

import Types
import Backprop

errorTotal :: [Double] -> [Double] -> Double
errorTotal xs ys = (1/2) * (sum $ zipWith (-) xs ys)**2

createNet :: [Int] -> Network
createNet xs = create (head xs) (tail xs)
    where
        create  _ [] = []
        create p (x:xs) = replicate x (Neuron (replicate (p + 1) 0.5) 0 0)
                            : create x xs

runNet :: [Double] -> Network -> [Double]
runNet xs net = map (\(Neuron _ o _) -> o) . last $ feedForward xs net

backprop :: [Double] -> [Double] -> Network -> Network
backprop xs ts net =
    let net' = calcHidError . calcOutError ts $ feedForward xs net
    in backpropError xs net'


train :: Int -> [[Double]] -> [[Double]] -> Network -> Network
train max xs ys net = run 0 net
    where
        run k net
            | k == max = net
            | otherwise = run (k + 1) $ epoch xs ys net

epoch :: [[Double]] -> [[Double]] -> Network -> Network
epoch [] [] net = net
epoch (xs:xss) (ys:yss) net =
    let net' = backprop xs ys net
    in epoch xss yss net'

-- Learn the XOR-operator

-- 1 inputs layer, 1 hidden layer, 1 output layer
net = createNet [2,3,1]

samples = [[0, 0], [0],
           [0, 1], [1],
           [1, 0], [1],
           [1, 1], [0]]

net' = train 10 xs ys net
    where
        xs = filter (\xs -> length xs == 2) samples
        ys = filter (\xs -> length xs == 1) samples

xor :: Bool -> Bool -> Bool
xor a b =
    let x = head $ runNet [toInt a, toInt b] net'
    in if x < 0.5
        then False
        else True
    where
        toInt b = if b then 1 else 0

someFunc :: IO ()
someFunc = putStrLn "someFunc"
