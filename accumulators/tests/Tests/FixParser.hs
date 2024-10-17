module Tests.FixParser where

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

import FixParser (isDigitByte)

prop_isDigitByte :: H.Property
prop_isDigitByte = H.property $ do
    -- generate a random Unsigned 8-bit integer (Byte)
    byte <- H.forAll (genUnsigned @8 Range.linearBounded)
    -- compute expected result
    let expected = byte >= 48 && byte <= 57  -- ASCII codes for '0' to '9'
        result = isDigitByte byte
    -- check that the result matches expected
    result === expected

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
