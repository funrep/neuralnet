module Numeric.ANN.Types where

import Data.Vector (Vector)

type Network = Vector Layer

type Layer = Vector Neuron

-- Seperate bias from weights?
data Neuron = Neuron (Vector Weight) Output Error
    deriving (Show, Eq)

type Weight = Double

type Output = Double

type Error = Double

type SampleData = Vector (Vector Double, Vector Double)
