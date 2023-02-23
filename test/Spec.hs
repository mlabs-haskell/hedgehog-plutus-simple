import Hedgehog.Main (defaultMain)

import Time (timeTests)

main :: IO ()
main =
  defaultMain
    [ timeTests
    ]
