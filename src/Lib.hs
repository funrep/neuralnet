module Lib where

data TrainingSet = TrainingSet [[Double]] [Double]
    deriving Show

andData :: TrainingSet
andData = TrainingSet [[1, 1], [0, 1], [1, 0], [0, 0]] [1, 0, 0, 0]

net = createNet [2,1]
net' = layer net 0
xs = [1.0, 1.0]
ys = feedForward xs net
ts = [1.0]

type Weight = Double

data Neuron = Neuron [Weight] deriving Show

data Network = Network [[Neuron]] deriving Show

layer :: Network -> Int -> [Neuron]
layer (Network ns) k = ns !! k

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + e**x)
    where e = exp 1

errorTotal :: [Double] -> [Double] -> Double
errorTotal xs ys = (1/2) * (sum $ zipWith (-) xs ys)**2

backprop :: [Double] -> [Double] -> [Double] -> [Neuron] -> [Neuron]
backprop xs ys ts = map (\(Neuron ws) ->
                            Neuron $ zipWith (+)
                                        (map (newWeight) (dws ws)) 
                                        ws)
    where
        dws ws = zipWith (dw (net ws)) (ext ws ts) (1:xs)
        net ws = sigmoid . sum $ zipWith (*) (1:xs) ws
        ext ws = concat . replicate (length ws)

dw :: Double -> Double -> Double -> Double
dw a t x = (t - a) * a * (1 - a) * x

learningRate :: Double
learningRate = 0.01

newWeight :: Double -> Double
newWeight dw = -n * dw
    where n = learningRate

createNet :: [Int] -> Network
createNet xs = Network $ create (head xs) (tail xs)
    where
        create  _ [] = []
        create p (x:xs) = replicate x (Neuron (replicate (p + 1) 0.5))
                            : create p xs

runNeuron :: [Double] -> Neuron -> Double
runNeuron xs (Neuron ws) = sigmoid . sum $ zipWith (*) (1:xs) ws

runLayer :: [Double] -> [Neuron] -> [Double]
runLayer xs ns = map (runNeuron xs) ns

feedForward :: [Double] -> Network -> [Double]
feedForward xs (Network ns) = run xs ns
    where
        run xs [] = xs
        run xs (n:ns) = run (runLayer xs n) ns

someFunc :: IO ()
someFunc = putStrLn "someFunc"
