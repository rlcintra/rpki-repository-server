import Test.Tasty
import Test.Tasty.Hspec

import ServerTest

main :: IO ()
main = do
  testTree <- testSuite
  defaultMain $ testGroup "RPKI Repository Test Suite" [testTree]

testSuite :: IO TestTree
testSuite = testSpec "Server tests" serverTests