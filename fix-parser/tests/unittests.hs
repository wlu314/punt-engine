import Test.Tasty
import qualified Tests.FixParser
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.FixParser.tests
      ]
