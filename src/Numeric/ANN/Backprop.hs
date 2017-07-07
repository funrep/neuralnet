module Numeric.ANN.Backprop (
    backprop,
    feedForward,
    ) where


import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe

import Numeric.ANN.Types

-- Utilities
outputs :: Vector Neuron -> Vector Double
outputs = fmap (\(Neuron _ o _) -> o)

toNeurons :: Vector Double -> Vector Neuron
toNeurons = fmap (\x -> Neuron V.empty x 0)

-- The backprop algorithm
backprop :: Double -> Vector Double -> Vector Double -> Network -> Network
backprop l xs ts =
    backpropError l xs . calcHidError . calcOutError ts . feedForward xs

-- Calculate activations values
feedForward :: Vector Double -> Network -> Network
feedForward xs = V.tail . V.scanl runLayer (toNeurons xs)
    where
        runLayer ps ns = fmap (calcOutput (outputs ps)) ns

calcOutput :: Vector Double -> Neuron -> Neuron
calcOutput xs (Neuron ws _ e) = Neuron ws out e
    where
        out = sigmoid . V.sum $ V.zipWith (*) (1 `V.cons` xs) ws
        sigmoid x = 1 / (1 + (exp 1)**(-x))

-- Calculate error signals of output layer
calcOutError :: Vector Double -> Network -> Network
calcOutError ts nss =
    V.init nss V.++ 
        V.singleton(V.zipWith (\t (Neuron ws o _) -> Neuron ws o (err t o)) ts $ V.last nss)
    where err t o = (t - o) * o * (1 - o)

-- Calculate error signals for hidden layers
calcHidError :: Network -> Network
calcHidError nss = V.scanr1 runLayer nss
    where
        runLayer ns ps = fmap (\(Neuron ws o _) -> Neuron ws o $ err o ns ps) ns
        err o ns ps = o * (1 - o) * (errSum ns ps)
        errSum ns ps = V.foldl' (\k (Neuron ws _ e) -> k + getWeight ns ws * e) 0 ps
        getWeight ns ws = V.tail ws V.! (fromJust $ V.elemIndex ns nss) -- ugly ugly code

-- Back propagate errors
backpropError :: Double -> Vector Double -> Network -> Network
backpropError l xs nss = V.zipWith runLayer nss (toNeurons xs `V.cons` V.init nss)
    where
        runLayer ns ps = fmap (\(Neuron ws o e) -> Neuron (update e (outputs ps) ws) o e) ns
        update e xs ws = updateBias (V.head ws) e `V.cons` updateWeights l e xs (V.tail ws)
        updateBias b e = b + l * e
 
updateWeights :: Double -> Error -> Vector Output -> Vector Weight -> Vector Weight
updateWeights l e xs ws
    | V.null ws = V.empty 
    | otherwise = 
        let dWeight = l * e * (V.head xs)
        in (V.head ws + dWeight) `V.cons` updateWeights l e (V.tail xs) (V.tail ws)
