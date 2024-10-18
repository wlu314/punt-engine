import Test.Tasty
import qualified Tests.Accumulators
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.Accumulators.tests
      ]
