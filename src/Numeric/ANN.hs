module Numeric.ANN
    ( Network
    , SampleData
    , createNet
    , runNet
    , train
    , trainTo
    , createSample
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random.Stateful
import Control.Monad

import Numeric.ANN.Types
import Numeric.ANN.Backprop

-- | Creates a neural network.
createNet ::
    RandomGenM g r m =>
    -- | List representing number of neurons in each layer
    [Int] ->
    -- | Random number generator
    g ->
    -- | Resulting neural network
    m Network
createNet xs = create (head xs) (tail xs)
    where
        create _ [] g = return V.empty
        create p (x:xs) g = do
            ns <- V.replicateM x (neuron p g)
            fmap (V.cons ns) (create x xs g)
        neuron p g = do
            ws <- V.replicateM (p + 1) $ randomRM (-1.0, 1.0) g
            return $ Neuron ws 0 0

-- | Run neural network on some input.
runNet :: [Double] -> Network -> Vector Double
runNet xs = outputs . V.last . feedForward (V.fromList xs)

-- | Train neural network.
train ::
    -- | Maximum iterations
    Int ->
    -- | Learning rate
    Double ->
    -- | Training data
    SampleData ->
    -- | Neural network
    Network ->
    -- | Updated network
    Network
train max learningRate xss = run max
    where
        run 0 nss = nss
        run k nss = run (k - 1) $ epoch learningRate xss nss

-- | Train neural network until certain precision.
trainTo :: 
    -- | Maximum iterations
    Int ->
    -- | Learning rate
    Double ->
    -- | Minimum error to halt training
    Double ->
    -- | Training data
    SampleData ->
    -- | Neural network
    Network ->
    -- | Updated network
    Network
trainTo max learningRate error xss = run max
    where
        run k nss
            | k == 0 = nss
            | overallError xss nss < error = nss
            | otherwise = run (k - 1) $ epoch learningRate xss nss

overallError :: SampleData -> Network -> Double
overallError xss nss =
    V.sum $ fmap (\(xs, ys) -> errorTotal (runNet (V.toList xs) nss) ys) xss

errorTotal :: Vector Double -> Vector Double -> Double
errorTotal xs ys = (1/2) * (V.sum . fmap (**2) $ V.zipWith (-) xs ys)

epoch :: Double -> SampleData -> Network -> Network
epoch l xss nss = V.foldl' (\nss (xs, ys) -> backprop l xs ys nss) nss xss

createSample :: [([Double], [Double])] -> SampleData
createSample = V.fromList . fmap (\(xs, ys) -> (V.fromList xs, V.fromList ys))
