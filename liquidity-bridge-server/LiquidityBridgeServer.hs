{-# LANGUAGE RecordWildCards #-}

module LiquidityBridgeServer (main) where

import Control.Exception (Exception, SomeException (..), catch, throw)
import Control.Exception.Safe (Handler (..), catches)
import Control.Monad (unless, void, when)
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Data.Text (pack)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import LiquidityBridgeServer.CardanoRequests (CardSubscription)
import LiquidityBridgeServer.Config (Config, Endpoint, getConfig)
import LiquidityBridgeServer.EthereumInterface (ethereumSetup)
import LiquidityBridgeServer.EthereumRequests (EthSubscription)
import LiquidityBridgeServer.Requests qualified as R
import Network.WebSockets (Connection, ConnectionException (..), runClient, sendClose)
import System.Timeout (timeout)
import Prelude

{- | Predicate for errors thrown when websocket connection fails

 @since 0.1
-}
isCouldntConnectError :: IOException -> Bool
isCouldntConnectError IOError {..} =
  isNothing ioe_handle
    && ioe_type == NoSuchThing
    && take 33 ioe_location == "Network.Socket.connect: <socket: " -- The line number after socket: seems to change arbitrarily, so we don't check it
    && ioe_description == "Connection refused"
    && ioe_errno == Just 111
    && isNothing ioe_filename

{- | Predicate for errors throw when a connection closes

 @since 0.1
-}
isConnectionClosedError :: ConnectionException -> Bool
isConnectionClosedError = (== ConnectionClosed)

{- | Error logs and service checks for a failed connection

 @since 0.1
-}
failedConnection :: Config -> String -> IO ()
failedConnection config str = do
  putStrLn str
  putStrLn "Identifying down service..."
  cardanoStatus <- isServiceUp config.cardanoEndpoint
  ethereumStatus <- isServiceUp config.ethereumEndpoint
  putStrLn $ "  Cardano PAB: " ++ if cardanoStatus then "UP" else "DOWN"
  putStrLn $ "  OpenEthereum: " ++ if ethereumStatus then "UP" else "DOWN"

{- | Error logs for any other caught error

 @since 0.1
-}
otherException :: forall (e :: Type). Exception e => Config -> e -> IO ()
otherException config e = do
  putStrLn "Caught exception:"

  -- TODO: We get a bad file descriptor error if we try to wait or getLine from here
  -- Calling the isServiceDown endpoint however seems to fix it?
  -- Theres very little info online about the issue, this is temporary fix
  void $ isServiceUp config.cardanoEndpoint

  print e

{- | Picks appropriate handle from above based on given exception predicate

 @since 0.1
-}
errorCondition :: forall (e :: Type). (Exception e) => Config -> (e -> Bool) -> String -> e -> IO ()
errorCondition config f str e
  | f e = failedConnection config str
  | otherwise = otherException config e

{- | Get config once at startup, pass to connect loop

 @since 0.1
-}
main :: IO ()
main = do
  config <- getConfig
  run config

{- | Loop connecting on error, printing the exception
  We overwrite return ()'s to be booleans, for checking success

 @since 0.1
-}
run :: Config -> IO ()
run config = do
  finished <-
    (True <$ connect config)
      `catches` [ Handler $ (False <$) . errorCondition @IOException config isCouldntConnectError "Couldn't connect."
                , Handler $ (False <$) . errorCondition @ConnectionException config isConnectionClosedError "Connection closed."
                , Handler $ (False <$) . otherException @SomeException config
                ]

  unless finished $ do
    putStrLn "Retrying in 5 seconds... (press enter to cancel)"
    shouldResume <- (== Nothing) <$> timeout 5_000_000 getLine

    when shouldResume $ run config

{- | Attempts to create a websocket connection then close it immediately. Used to check if service is running.

 @since 0.1
-}
isServiceUp :: Endpoint -> IO Bool
isServiceUp endpoint =
  catch @SomeException
    (runClient endpoint.url endpoint.port endpoint.path close >> return True)
    (const $ return False)

{- | Closes a websocket

 @since 0.1
-}
close :: Connection -> IO ()
close c = sendClose c $ pack "Bye!"

{- | Closes a connection and discards any errors

 @since 0.1
-}
closeRegardless :: Connection -> IO ()
closeRegardless c = catch @SomeException (close c) (const $ return ())

{- | Runs handleConnections, closing both connections upon any error and forwarding said error

 @since 0.1
-}
runHandleConnections :: Config -> Connection -> Connection -> IO ()
runHandleConnections config cardanoWS ethereumWS = catch @SomeException
  (handleConnections config cardanoWS ethereumWS)
  $ \e -> do
    putStrLn "Error caught, forcefully closing connections!"
    closeRegardless cardanoWS
    closeRegardless ethereumWS
    throw e

{- | Connect WS to Cardano and Ethereum and pass endpoints to handleConnections

 @since 0.1
-}
connect :: Config -> IO ()
connect config = do
  putStrLn "Connecting..."
  let card :: Endpoint
      card = config.cardanoEndpoint
      eth :: Endpoint
      eth = config.ethereumEndpoint
  runClient card.url card.port card.path $ \cardanoWS ->
    runClient eth.url eth.port eth.path $ \ethereumWS ->
      runHandleConnections config cardanoWS ethereumWS

handleConnections :: Config -> Connection -> Connection -> IO ()
handleConnections config cardanoWS ethereumWS = do
  putStrLn "Connected! Press enter to close."

  putStrLn "Starting loops"
  ethRc <- R.startLoop @EthSubscription ethereumWS
  cardRc <- R.startLoop @CardSubscription cardanoWS

  -- Set up listeners
  ethereumSetup config ethRc cardRc

  -- Hold open
  void getLine

  -- Cleanup
  putStrLn "Closing connections."
  R.stopLoop ethRc
  R.stopLoop cardRc
  closeRegardless cardanoWS
  closeRegardless ethereumWS
  putStrLn "Disconnected!"
