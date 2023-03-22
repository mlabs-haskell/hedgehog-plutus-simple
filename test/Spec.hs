import Test.Tasty

import Time (timeTests)
import TxValid (txValidTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "implementation tests"
      [ timeTests
      , txValidTests
      ]
