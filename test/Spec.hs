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
    [ netTest "[2-1 net OR-data]" net21 orData
    , netTest "[2-1 net AND-data]" net21 andData
    , netTest "[2-3-1 net OR-data]" net231 orData
    , netTest "[2-3-1 net AND-data]" net231 andData
    , netTest "[2-3-1 net XOR-data]" net231 xorData
    ]

netTest :: String -> Network -> [([Double], [Double])] -> TestTree
netTest s net data' = testGroup s $ map test data'
    where
        net' = train 10000 0.1 (createSample data') net
        test (input, expected) = testCase (show input) $
            (V.head $ runNet input net') @?~== head expected

net21, net231 :: Network
net21 = runStateGen_ (mkStdGen 1) $ createNet [2, 1]
net231 = runStateGen_ (mkStdGen 2) $ createNet [2, 3, 1]

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
