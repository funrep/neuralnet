module Backprop where

import Data.List
import Data.Maybe

import Types

-- Calculate activations values
feedForward :: [Double] -> Network -> Network
feedForward _ [] = []
feedForward xs (ns:nss) =
    let ns' = map (runNeuron xs) ns
    in ns' : feedForward (outputs ns') nss
    where outputs = map (\(Neuron _ o _) -> o)

runNeuron :: [Double] -> Neuron -> Neuron
runNeuron xs (Neuron ws _ e) = Neuron ws out e
    where out = sigmoid . sum $ zipWith (*) (1:xs) ws

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + e**(-x))
    where e = exp 1

-- Calculate error signals of output layer
calcOutError :: [Double] -> Network -> Network
calcOutError ts nss =
    take (length nss - 1) nss ++ [run ts $ last nss]
    where
        run _ [] = []
        run (t:ts) ((Neuron ws o _):ns) =
            (Neuron ws o (err t o)) : run ts ns
        err t o = (t - o) * o * (1 - o)

-- Calculate error signals for hidden layers
calcHidError :: Network -> Network
calcHidError = reverse . calcHidErrorRev . reverse

calcHidErrorRev :: Network -> Network
calcHidErrorRev old@(ns:nss) = ns : run ns nss
    where
        run _ [] = []
        run ps (ns:nss) =
            map (\(Neuron ws o e) -> Neuron ws o $ err o e ps ns) ns
                : run ns nss
        err o e ps ns = o * (1 - o) * (errSum ps ns)
        errSum ps ns = sum $ map (\(Neuron ws _ e) ->
                                ((tail ws) !! index ns) * e) ps
        index ns = fromJust $ elemIndex ns old

-- calcHidErrorRev :: Network -> Network
-- calcHidErrorRev
-- calcHidErrorRev (ns:nss)

-- Back propagate errors
backpropError :: [Double] -> Network -> Network
backpropError xs = reverse . backpropErrorRev xs . reverse

backpropErrorRev :: [Double] -> Network -> Network
backpropErrorRev _ [] = []
backpropErrorRev xs (ns:nss)
    | nss == [] = [updateLayer xs ns]
    | otherwise = updateLayer (outputs nss) ns
                    : backpropError xs nss
    where outputs = map (\(Neuron _ o _) -> o) . head

updateLayer :: [Double] -> Layer -> Layer
updateLayer xs ns =
    map (\(Neuron ws o e) -> Neuron (update e xs ws) o e) ns
    where
        update e xs (w:ws) = updateBias w e : updateWeights e xs ws
        updateBias b e = b + learningRate * e

updateWeights :: Error -> [Output] -> [Weight] -> [Weight]
updateWeights _ _ [] = []
updateWeights e (x:xs) (w:ws) =
    let dWeight = learningRate * e * x
    in w + dWeight : updateWeights e xs ws

learningRate :: Double
learningRate = 0.2
