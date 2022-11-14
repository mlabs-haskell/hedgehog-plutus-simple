module Hedgehog.Plutus.Simple(
  testNoErrors,
  testNoErrorsTrace,
  testLimits
  ) where

import PlutusLedgerApi.V1.Value(Value)
import Plutus.Model(MockConfig, Run, checkErrors, runMock, initMock, Mock, ppMockEvent, mockNames, getLog, Log, TxStat, warnLimits, ppLimitInfo, mockTxs)

import Hedgehog(success, footnote, failure, PropertyT, evalIO, assert)
import Data.Maybe (isJust, isNothing)

testNoErrors :: Value -> MockConfig -> Run a -> PropertyT IO ()
testNoErrors funds cfg act =
  case fst (runAction funds cfg act) of
    Nothing -> success 
    Just err -> footnote err >> failure 

testNoErrorsTrace :: Value -> MockConfig -> Run a -> PropertyT IO ()
testNoErrorsTrace funds cfg act = do
  evalIO $ do
    putStrLn ""
    putStrLn "Blockchain log:"
    putStrLn "----------------"
    putStrLn $ ppMockEvent (mockNames bch) (getLog bch)
  assert $ isJust errors
  where
    (errors, bch) = runAction funds cfg act

testLimits :: Value -> MockConfig -> (Log TxStat -> Log TxStat) -> Run a -> PropertyT IO ()
testLimits initFunds cfg tfmLog act =
  if isNothing errors
    then success
    else footnote (ppLimitInfo (mockNames bch) (tfmLog $ mockTxs bch)) >> failure
  where
    (errors, bch) = runAction initFunds (warnLimits cfg) act


runAction :: Value -> MockConfig -> Run a -> (Maybe String, Mock)
runAction funds cfg act = runMock(act >> checkErrors) (initMock cfg funds)