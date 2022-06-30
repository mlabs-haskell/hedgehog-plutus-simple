module Hedgehog.Plutus.Simple(
  testNoErrors,
  testNoErrorsTrace,
  testLimits
  ) where

import Plutus.V1.Ledger.Value(Value)
import Plutus.Test.Model(BchConfig, Run, checkErrors, runBch, initBch, Blockchain, ppBchEvent, bchNames, getLog, Log, TxStat, warnLimits, ppLimitInfo, bchTxs)

import Hedgehog(success, footnote, failure, PropertyT, evalIO, assert)
import Data.Maybe (isJust, isNothing)

testNoErrors :: Value -> BchConfig -> Run a -> PropertyT IO ()
testNoErrors funds cfg act =
  case fst (runAction funds cfg act) of
    Nothing -> success 
    Just err -> footnote err >> failure 

testNoErrorsTrace :: Value -> BchConfig -> Run a -> PropertyT IO ()
testNoErrorsTrace funds cfg act = do
  evalIO $ do
    putStrLn ""
    putStrLn "Blockchain log:"
    putStrLn "----------------"
    putStrLn $ ppBchEvent (bchNames bch) (getLog bch)
  assert $ isJust errors
  where
    (errors, bch) = runAction funds cfg act

testLimits :: Value -> BchConfig -> (Log TxStat -> Log TxStat) -> Run a -> PropertyT IO ()
testLimits initFunds cfg tfmLog act =
  if isNothing errors
    then success
    else footnote (ppLimitInfo (bchNames bch) (tfmLog $ bchTxs bch)) >> failure
  where
    (errors, bch) = runAction initFunds (warnLimits cfg) act


runAction :: Value -> BchConfig -> Run a -> (Maybe String, Blockchain)
runAction funds cfg act = runBch (act >> checkErrors) (initBch cfg funds)