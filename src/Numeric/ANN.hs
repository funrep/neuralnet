module Numeric.ANN (
    Network,
    createNet,
    runNet,
    train,
    trainTo,
    createSample,
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random

import Numeric.ANN.Types
import Numeric.ANN.Backprop

createNet :: [Int] -> IO Network
createNet xs = create (head xs) (tail xs)
    where
        create _ [] = return V.empty
        create p (x:xs) = do
            ns <- V.replicateM x (neuron p)
            fmap (V.cons ns) (create x xs)
        neuron p = do
            ws <- V.replicateM (p + 1) (randomRIO (-1, 1))
            return (Neuron ws 0 0)

runNet :: [Double] -> Network -> Vector Double
runNet xs = fmap (\(Neuron _ o _) -> o) . V.last . feedForward (V.fromList xs)

train :: Int -> Double -> SampleData -> Network -> Network
train max learningRate xss = run max
    where
        run 0 nss = nss
        run k nss = run (k - 1) $ epoch learningRate xss nss

trainTo :: Int -> Double -> Double -> SampleData -> Network -> Network
trainTo max learningRate error xss = run max
    where
        run k nss
            | k == 0 = nss
            | overallError xss nss < error = nss
            | otherwise = run (k - 1) $ epoch learningRate xss nss

overallError :: SampleData -> Network -> Double
overallError xss nss =
    V.sum $ fmap (\(xs, ys) -> errorTotal (runNet (V.toList xs) nss) ys) xss

errorTotal :: Vector Double -> Vector Double -> Double
errorTotal xs ys = (1/2) * (V.sum . fmap (**2) $ V.zipWith (-) xs ys)

epoch :: Double -> SampleData -> Network -> Network
epoch l xss nss = V.foldl' (\nss (xs, ys) -> backprop l xs ys nss) nss xss

createSample :: [([Double], [Double])] -> SampleData
createSample = V.fromList . fmap (\(xs, ys) -> (V.fromList xs, V.fromList ys))

