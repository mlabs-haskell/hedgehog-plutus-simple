import Hedgehog (checkParallel)
import Hedgehog.Main (defaultMain)

import Time (timeTests)
import TxValid (txValidTests)

main :: IO ()
main =
  defaultMain
    [ checkParallel timeTests
    , checkParallel txValidTests
    ]
