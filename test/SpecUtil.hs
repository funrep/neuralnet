module SpecUtil (assertApproxEqual, (@?~==))where

import Control.Monad (unless)
import Test.Tasty.HUnit (Assertion, assertFailure)

assertApproxEqual :: (Show a, Fractional a, Ord a) => String -> a -> a -> Assertion
assertApproxEqual preface expected actual =
    unless
      (actual >= expected - 0.1 && actual <= expected + 0.1)
      (assertFailure msg)
    where
        msg =
            (if null preface then "" else preface ++ "\n")
            ++ "expected: "
            ++ show expected
            ++ "\n but got: "
            ++ show actual

(@?~==) :: (Show a, Fractional a, Ord a) => a -> a -> Assertion
(@?~==) actual expected = assertApproxEqual "" expected actual
infix 1 @?~==
