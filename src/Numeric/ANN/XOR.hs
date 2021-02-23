module Numeric.ANN.XOR (xorNet) where

import System.Random.Stateful

import Numeric.ANN

samples = [([0, 0], [0]),
           ([0, 1], [1]),
           ([1, 0], [1]),
           ([1, 1], [0])]

xorNet :: IO ANN
xorNet = do
    net <- createNetIO 42 [2,3,1]
    let (Just net') = train 10000 0.2 (createSample samples) net
    return net'
