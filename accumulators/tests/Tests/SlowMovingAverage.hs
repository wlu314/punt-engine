module Tests.SlowMovingAverage where

import Prelude
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import Hedgehog ((===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)

import SlowMovingAverage ()

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
