module Lib
    ( someFunc,
      createNet,
      feedForward,
      Network,
      Neuron,
      Weight
    ) where

type Weight = Double

data Neuron = Neuron [Weight] deriving Show

data Network = Network [[Neuron]] deriving Show

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + e**x)
    where e = exp 1

createNet :: [Int] -> Network
createNet = Network . create
    where
        create [] = []
        create (x:xs) = (replicate x $ Neuron (replicate x 0.5)) : create xs

runNeuron :: [Double] -> Neuron -> Double
runNeuron xs (Neuron ws) = sigmoid . sum $ zipWith (*) xs ws

runLayer :: [Double] -> [Neuron] -> [Double]
runLayer _ [] = []
runLayer xs (n:ns) = runNeuron xs n : runLayer xs ns

feedForward :: [Double] -> Network -> [Double]
feedForward xs (Network ns) = run xs ns
    where
        run xs [] = xs
        run xs (n:ns) = run (runLayer xs n) ns

someFunc :: IO ()
someFunc = putStrLn "someFunc"
