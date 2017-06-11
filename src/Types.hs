module Types where

type Weight = Double

type Error = Double

type Output = Double

data Neuron = Neuron [Weight] Output Error deriving Show

type Layer = [Neuron]

type Network = [Layer]