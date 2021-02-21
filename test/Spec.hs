import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random.Stateful

import SpecUtil

import Numeric.ANN

main :: IO ()
main = defaultMain allTests

allTests :: TestTree 
allTests = testGroup "All Tests" 
    [ annTests
    ]

annTests :: TestTree
annTests = testGroup "ann"
    [ netTest "[2-1 net OR-data]" net1 orData
    , netTest "[2-1 net AND-data]" net1 andData
    , netTest "[2-3-1 net OR-data]" net2 orData
    , netTest "[2-3-1 net AND-data" net2 andData
    , netTest "[2-3-1 net XOR-data" net2 xorData
    ]

netTest :: String -> Network -> [([Double], [Double])] -> TestTree
netTest s net data' = testGroup s $ map test data'
    where
        test (input, expected) = testCase (show input) $
            (V.head $ trainAndRun (createSample data') net input) @?~== head expected

trainAndRun :: SampleData -> Network -> [Double] -> Vector Double
trainAndRun data' net input =
    runNet input $
        train 10 0.1 data' net

net1, net2 :: Network
net1 = runStateGen_ (mkStdGen 1) $ createNet [2, 1]
net2 = runStateGen_ (mkStdGen 2) $ createNet [2, 3, 1]

orData, andData, xorData :: [([Double], [Double])]
orData =
  [ ([0, 0], [0])
  , ([0, 1], [1])
  , ([1, 0], [1])
  , ([1, 1], [1])
  ]
andData =
  [ ([0, 0], [0])
  , ([0, 1], [0])
  , ([1, 0], [0])
  , ([1, 1], [1])
  ]
xorData =
  [ ([0, 0], [0])
  , ([0, 1], [1])
  , ([1, 0], [1])
  , ([1, 1], [0])
  ]
