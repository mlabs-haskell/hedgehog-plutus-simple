module Main(main) where

import Auction.StateMachine(auctionTests)
import Hedgehog (checkSequential)
import Control.Monad (void)

main :: IO ()
main = void $ checkSequential auctionTests
