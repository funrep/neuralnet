{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import System.Random.Stateful
import Data.Maybe

import SpecUtil ((@?~==))

import Numeric.ANN
import Data.MNIST

main :: IO ()
main = defaultMain allTests

allTests :: TestTree 
allTests = testGroup "All Tests" 
    [ mnistTests
    , annTests
    ]

mnistTests :: TestTree
mnistTests = testGroup "mnist"
    [ testCase "readInteger empty" $ readInteger empty @?= Nothing
    , testCase "readInteger 1337" $ readInteger int1337 @?= Just (1337, "")
    , testCase "readInteger with rest" $ readInteger ints @?= Just (1, rest)
    , testCase "readBytes empty" $ readBytes empty @?= []
    , testCase "readBytes 0, 0, 5, 57" $ readBytes int1337 @?= [0, 0, 5, 57]
    ]
    where
        empty = ""
        -- 1337 == 0x539 == 0 0 5 3*16+9 == 0 0 5 57 
        int1337 = 0 `BS.cons` 0 `BS.cons` 5 `BS.cons` 57 `BS.cons` ""
        ints = 0 `BS.cons` 0 `BS.cons` 0 `BS.cons` 1 `BS.cons` 1 `BS.cons` ""
        rest = 1 `BS.cons` ""

annTests :: TestTree
annTests = testGroup "ann"
    [ netTest "[2-1 net OR-data]" net21 orData
    , netTest "[2-1 net AND-data]" net21 andData
    , netTest "[2-3-1 net OR-data]" net231 orData
    , netTest "[2-3-1 net AND-data]" net231 andData
    , netTest "[2-3-1 net XOR-data]" net231 xorData
    ]

netTest :: String -> ANN -> [([Double], [Double])] -> TestTree
netTest s net data' = testGroup s $ map test data'
    where
        net' = train 10000 0.1 (createSample data') net
        test (input, expected) = testCase (show input) $
            fromJust (fmap V.head $ runNet input =<< net') @?~== (head expected)

net21, net231 :: ANN
net21 = runStateGen_ (mkStdGen 1) $ flip createNet [2, 1]
net231 = runStateGen_ (mkStdGen 2) $ flip createNet [2, 3, 1]

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
