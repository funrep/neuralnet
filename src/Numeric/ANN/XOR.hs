module Numeric.ANN.XOR (xorNet) where

import Numeric.ANN

samples = [([0, 0], [0]),
           ([0, 1], [1]),
           ([1, 0], [1]),
           ([1, 1], [0])]

xorNet :: IO Network
xorNet = do
    net <- createNet [2,3,1]
    let net' = train 10000 0.2 (createSample samples) net
    return net'
