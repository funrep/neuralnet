module Numeric.ANN.Types where

import Data.Vector (Vector)

data ANN = ANN
    { _layers :: Network
    , _dims :: [Int]
    }

type Network = Vector (Vector Neuron)

data Neuron = Neuron (Vector Weight) Output Error
    deriving (Show, Eq)

type Weight = Double

type Output = Double

type Error = Double

type SampleData = Vector (Vector Double, Vector Double)
