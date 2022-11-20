module Hedgehog.Plutus.Simple (
  testNoErrors,
  testNoErrorsTrace,
  testLimits,
) where

import Plutus.Model (
  Log,
  Mock,
  MockConfig,
  Run,
  TxStat,
  checkErrors,
  getLog,
  initMock,
  mockNames,
  mockTxs,
  ppLimitInfo,
  ppMockEvent,
  runMock,
  warnLimits,
 )
import PlutusLedgerApi.V1.Value (Value)

import Data.Kind (Type)
import Data.Maybe (isJust, isNothing)
import Hedgehog (PropertyT, assert, evalIO, failure, footnote, success)

testNoErrors ::
  forall (a :: Type). Value -> MockConfig -> Run a -> PropertyT IO ()
testNoErrors funds cfg act =
  case fst (runAction funds cfg act) of
    Nothing -> success
    Just err -> footnote err >> failure

testNoErrorsTrace ::
  forall (a :: Type). Value -> MockConfig -> Run a -> PropertyT IO ()
testNoErrorsTrace funds cfg act = do
  evalIO $ do
    putStrLn ""
    putStrLn "Blockchain log:"
    putStrLn "----------------"
    putStrLn $ ppMockEvent (mockNames bch) (getLog bch)
  assert $ isJust errors
  where
    (errors, bch) = runAction funds cfg act

testLimits ::
  forall (a :: Type).
  Value ->
  MockConfig ->
  (Log TxStat -> Log TxStat) ->
  Run a ->
  PropertyT IO ()
testLimits initFunds cfg tfmLog act =
  if isNothing errors
    then success
    else
      footnote (ppLimitInfo (mockNames bch) (tfmLog $ mockTxs bch))
        >> failure
  where
    (errors, bch) = runAction initFunds (warnLimits cfg) act

runAction ::
  forall (a :: Type). Value -> MockConfig -> Run a -> (Maybe String, Mock)
runAction funds cfg act = runMock (act >> checkErrors) (initMock cfg funds)
