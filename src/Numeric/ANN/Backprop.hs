module Numeric.ANN.Backprop
    ( backprop
    , feedForward
    , outputs
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

-- | The backprop algorithm.
backprop ::
    -- | Learning rate
    Double ->
    -- | Input vector
    Vector Double ->
    -- | Expected output
    Vector Double ->
    -- | Neural network
    Network ->
    -- | Updated network
    Network
backprop l xs ts =
    updateWeights l xs . backpropErrors . calcOutErrors ts . feedForward xs

-- | Calculate activations values.
feedForward ::
    -- | Input vector
    Vector Double ->
    -- | Neural network
    Network ->
    -- | Updated network
    Network
feedForward xs = V.tail . V.scanl runLayer (toNeurons xs)
    where
        runLayer ps ns = fmap (calcOutput (outputs ps)) ns

calcOutput :: Vector Double -> Neuron -> Neuron
calcOutput xs (Neuron ws _ e) = Neuron ws out e
    where
        out = sigmoid . V.sum $ V.zipWith (*) (1 `V.cons` xs) ws
        sigmoid x = 1 / (1 + (exp 1)**(-x))

-- | Calculate error signals of the output layer, using sigmoid's derivative.
calcOutErrors ::
    -- | Expected output
    Vector Double ->
    -- | Neural network
    Network ->
    -- | Updated network 
    Network
calcOutErrors ts nss =
    V.init nss V.++
        V.singleton (V.zipWith (\t (Neuron ws o _) -> Neuron ws o (err t o)) ts $ V.last nss)
    where err t o = (t - o) * o * (1 - o)

-- | Back propegate error signals for the hidden layers.
backpropErrors :: Network -> Network
backpropErrors nss = V.scanr1 runLayer nss
    where
        runLayer ns ps = fmap (\(Neuron ws o _) -> Neuron ws o $ err o ns ps) ns
        err o ns ps = (errSum ns ps) * o * (1 - o)
        errSum ns ps = V.foldl' (\k (Neuron ws _ e) -> k + getWeight ns ws * e) 0 ps
        getWeight ns ws = V.tail ws V.! (fromJust $ V.elemIndex ns nss)

updateWeights ::
    -- | Learning rate
    Double ->
    -- | Input vector
    Vector Double ->
    -- | Neural network
    Network ->
    -- | Updated network 
    Network
updateWeights l xs nss = V.zipWith runLayer nss (toNeurons xs `V.cons` V.init nss)
    where
        runLayer ns ps = fmap (\(Neuron ws o e) -> Neuron (update e ws (outputs ps)) o e) ns
        update e ws xs = V.zipWith (\w x -> w + l * e * x) ws (1 `V.cons` xs)
 