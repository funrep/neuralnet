module Numeric.ANN.XOR (xorNet) where

import System.Random.Stateful

import Numeric.ANN

samples = [([0, 0], [0]),
           ([0, 1], [1]),
           ([1, 0], [1]),
           ([1, 1], [0])]

xorNet :: IO Network
xorNet = do
    g <- newIOGenM $ mkStdGen 1
    net <- createNet [2,3,1] g
    let net' = train 10000 0.2 (createSample samples) net
    return net'
