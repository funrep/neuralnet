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
                            : create p xs

someFunc :: IO ()
someFunc = putStrLn "someFunc"
