{-# LANGUAGE TypeFamilies #-}

module LiquidityBridgeServer.Requests (
  Receiver (..),
  ReceiverData (..),
  ReceiverSubscription (..),
  addListener,
  addSubscriber,
  removeSubscriber,
  startLoop,
  stopLoop,
  waitLoop,
  printOnError,
) where

import Control.Concurrent (
  MVar,
  ThreadId,
  forkFinally,
  killThread,
  modifyMVar_,
  myThreadId,
  newEmptyMVar,
  putMVar,
  readMVar,
  takeMVar,
  throwTo,
  yield,
 )
import Control.Concurrent.Event (Event, new, signal, wait)
import Control.Exception (SomeAsyncException, SomeException (..))
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List (delete)
import Data.Typeable (Proxy (..), typeOf)
import Network.WebSockets (Connection, receiveData)
import Type.Reflection (someTypeRep)
import Prelude

{- | Class for subscriptions, capturing the creation and destruction of subscriptions,
  along with the expected request and error types

 @since 0.1
-}
class (Eq a, Show (ReceiverError a)) => ReceiverSubscription a where
  type ReceiverError a :: Type
  type ReceiverRequest a :: Type -> Type
  destroySubscriber :: Receiver a -> a -> IO ()
  createSubscriber ::
    forall (response :: Type) (request :: Type).
    (ToJSON request, FromJSON response) =>
    Receiver a ->
    ReceiverRequest a request ->
    (response -> IO ()) ->
    IO (Either (ReceiverError a) a)

{- | Adds a ReceiverSubscription to a Receiver, returning the given ReceiverSubscription or corresponding error type

 @since 0.1
-}
addSubscriber ::
  forall (response :: Type) (request :: Type) (a :: Type).
  (ToJSON request, FromJSON response, ReceiverSubscription a) =>
  Receiver a ->
  ReceiverRequest a request ->
  (response -> IO ()) ->
  IO (Either (ReceiverError a) a)
addSubscriber rc req callback = do
  es <- createSubscriber @a @response @request rc req callback
  case es of
    Right sub -> do
      modifyMVar_ rc.rMVar $ \recData ->
        return $
          recData
            { rdSubscriptions = sub : recData.rdSubscriptions
            }
      return $ Right sub
    _ -> return es

{- | Removes a ReceiverSubscription from a Receiver

 @since 0.1
-}
removeSubscriber ::
  forall (a :: Type).
  ReceiverSubscription a =>
  Receiver a ->
  a ->
  IO ()
removeSubscriber rc sub = do
  destroySubscriber @a rc sub
  modifyMVar_ rc.rMVar $ \recData ->
    return $
      recData
        { rdSubscriptions = delete sub recData.rdSubscriptions
        }

{- | Shared data for a receiver, to be stored in MVar

 @since 0.1
-}
data ReceiverData sub = ReceiverData
  { rdMsgs :: [ByteString]
  , rdMsgIdCounter :: Integer
  , rdReceiverThread :: ThreadId
  , rdSubscriptions :: [sub]
  }

{- | Default values for ReceiverData

 @since 0.1
-}
receiverData :: forall (a :: Type). ThreadId -> ReceiverData a
receiverData tId =
  ReceiverData
    { rdMsgs = []
    , rdMsgIdCounter = 0
    , rdReceiverThread = tId
    , rdSubscriptions = []
    }

{- | Receiver itself, holding shared var, new message event and connection

 @since 0.1
-}
data Receiver a = Receiver
  { rMVar :: MVar (ReceiverData a)
  , rEvent :: Event
  , rConnection :: Connection
  }

{- | Finds first element that meets a predicate, returns list without this value, and result of predicate

 @since 0.1
-}
filterFirstMaybe ::
  forall (a :: Type) (b :: Type).
  (a -> Maybe b) ->
  [a] ->
  ([a], Maybe b)
filterFirstMaybe _ [] = ([], Nothing)
filterFirstMaybe f (x : xs) = case f x of
  Just y -> (xs, Just y)
  Nothing -> (x : xs', y)
    where
      (xs', y) = filterFirstMaybe f xs

{- | Creates a fork and pushes any errors onto the parent thread

 @since 0.1
-}
forkForwardError :: forall (a :: Type). IO a -> IO ThreadId
forkForwardError c = do
  ownId <- myThreadId
  forkFinally c $ \case
    Left (SomeException e) | typeOf e /= someTypeRep (Proxy @SomeAsyncException) -> throwTo ownId e
    _ -> return ()

{- | Starts a websocket receiver loop in another thread, required for all transactions and subscriptions

 @since 0.1
-}
startLoop :: forall (a :: Type). Connection -> IO (Receiver a)
startLoop c = do
  mvar <- newEmptyMVar @(ReceiverData a)
  evt <- new
  tId <- forkForwardError . forever $ do
    msg <- receiveData @ByteString c
    modifyMVar_ mvar $ \recData -> return $ recData {rdMsgs = msg : recData.rdMsgs}
    signal evt
    yield

  putMVar mvar $ receiverData tId

  return $ Receiver mvar evt c

{- | Stops a receiver loop, and ends all current subscriptions

 @since 0.1
-}
stopLoop ::
  forall (a :: Type).
  ReceiverSubscription a =>
  Receiver a ->
  IO ()
stopLoop rc = do
  recData <- readMVar rc.rMVar
  mapM_ (removeSubscriber rc) recData.rdSubscriptions
  killThread recData.rdReceiverThread

{- | Waits until a message meeting a predicate is found, and returns the result of the predicate

 @since 0.1
-}
waitLoop ::
  forall (response :: Type) (a :: Type).
  FromJSON response =>
  Receiver a ->
  (ByteString -> Maybe (Either (ReceiverError a) response)) ->
  IO (Either (ReceiverError a) response)
waitLoop rc validator = do
  wait rc.rEvent
  recData <- takeMVar rc.rMVar
  let (xs', mv) = filterFirstMaybe validator recData.rdMsgs

  putMVar rc.rMVar (recData {rdMsgs = xs'})
  maybe (waitLoop rc validator) return mv

{- | Creates a looping waiter with a validator and callback to call on validated responses

 @since 0.1
-}
addListener ::
  forall (response :: Type) (a :: Type).
  FromJSON response =>
  Receiver a ->
  (ByteString -> Maybe (Either (ReceiverError a) response)) ->
  (response -> IO ()) ->
  IO ThreadId
addListener rc validator callback = forkForwardError . forever $ do
  res <- waitLoop rc validator
  case res of
    Left _ -> return ()
    Right v -> callback v

{- | Runs an errorable IO computation, prints the error if it occurs

 @since 0.1
-}
printOnError ::
  forall (a :: Type) (e :: Type).
  (Show e) =>
  IO (Either e a) ->
  IO ()
printOnError comp = do
  res <- comp
  putStrLn $ case res of
    Left e -> "Failure: " ++ show e
    _ -> "Success!"
