module Main (main) where

import Auction.StateMachine (auctionTests)
import Control.Monad (void)
import Hedgehog (checkSequential)

main :: IO ()
main =
  void $
    checkSequential auctionTests
