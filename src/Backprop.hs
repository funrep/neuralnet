module Backprop where

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
sigmoid x = 1 / (1 + e**x)
    where e = exp 1

-- Calculate error signals of output layer
calcOutError :: [Double] -> [Double] -> Network -> Network
calcOutError ts ys nss =
    take (length nss - 1) nss ++ [run ts ys $ last nss]
    where
        run _ _ [] = []
        run (t:ts) (y:ys) ((Neuron ws o _):ns) =
            (Neuron ws o (err t y)) : run ts ys ns
        err t y = (t - y) * y * (1 - y)

-- Calculate error signals for hidden layers
calcHidError :: Network -> Network
calcHidError = reverse . calcHidErrorRev . reverse

calcHidErrorRev :: Network -> Network
calcHidErrorRev (ns:nss) = run ns $ tail nss
    where
        run _ [] = []
        run ps (ns:nss) =
            map (\(Neuron ws o e) -> Neuron ws o $ err o e ps) ns
                : run ns nss
        err o e ns = o * (1 - o) * (errSum e ns)
        errSum e ns = sum $ map (\(Neuron ws _ e) ->
                                foldr (\w1 w2 -> w1 * e + w2 * e) 0 ws) ns

-- Back propagate errors
backpropError :: Network -> Network
backpropError = reverse . backpropErrorRev . reverse

backpropErrorRev :: Network -> Network
backpropErrorRev (ns:nss) = run ns nss
    where
        run _ [] = []
        run ps (ns:nss) = updateLayer ps ns : run ns nss

updateLayer :: Layer -> Layer -> Layer
updateLayer ps ns =
    map (\(Neuron ws o e) -> Neuron (update e ps ws) o e) ns
    where
        update e ps (w:ws) = updateBias w e : updateWeights e ps ws
        updateBias b e = b + learningRate * e

updateWeights :: Error -> Layer -> [Weight] -> [Weight]
updateWeights _ _ [] = []
updateWeights e ((Neuron _ o _ ):ps) (w:ws) =
    w + dWeight : updateWeights e ps ws
    where dWeight = learningRate * e * o

learningRate :: Double
learningRate = 0.01
