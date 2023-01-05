{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Auction.StateMachine (
  auctionTests,
  auctionTest,
) where

import Hedgehog (
  Callback (Ensure, Require, Update),
  Command (Command),
  Concrete (Concrete),
  FunctorB,
  Group (Group),
  MonadGen,
  Property,
  PropertyT,
  Rec (Rec),
  Symbolic,
  TraversableB,
  Var (Var),
  annotateShow,
  concrete,
  executeSequential,
  failure,
  forAll,
  property,
  (===),
 )
import Hedgehog.Internal.Distributive (MonadTransDistributive (distributeT))

import Control.Arrow (second)
import Control.Monad (guard, liftM2)
import Control.Monad.State.Strict (StateT, evalStateT, runState, state)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map (Map)
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import Plutus.Model (
  Mock,
  Run (Run),
  adaValue,
  checkErrors,
  defaultBabbage,
  initMock,
 )
import PlutusLedgerApi.V1 (PubKeyHash)
import PlutusLedgerApi.V1.Contexts (TxOutRef)

import Auction.PSM qualified as PSM
import Data.Map qualified as Map
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range qualified as Range

newtype User = User {name :: String}
  deriving stock (Eq, Ord, Show)

data AuctionState v = AuctionState
  { currentBid :: Maybe (User, User, Int, Var TxOutRef v)
  , -- auction owner, current bidder, bid, current utxo
    winner :: Maybe User
  , users :: Map User (Int, Var PubKeyHash v)
  }
  deriving stock (Eq, Ord, Show)

initialState :: AuctionState v
initialState =
  AuctionState
    { currentBid = Nothing
    , winner = Nothing
    , users = Map.empty
    }

data AddUser v
  = AddUser User Int
  deriving stock (Eq, Show, Generic)

instance FunctorB AddUser
instance TraversableB AddUser

addUser :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
addUser =
  let gen :: AuctionState Symbolic -> Maybe (m (AddUser Symbolic))
      gen s =
        Just $
          AddUser
            <$> Gen.filterT
              (\user -> user `notElem` Map.keys (users s))
              (User <$> Gen.list (Range.linear 0 100) Gen.alpha)
            <*> Gen.int (Range.linear 1_000_000 1_000_000_000)

      execute :: AddUser Concrete -> PropertyT RunIO PubKeyHash
      execute (AddUser _user startBal) = liftRun $ PSM.addUser startBal
   in Command
        gen
        execute
        [ Update $ \s (AddUser user startBal) pkh ->
            s
              { users = Map.insert user (startBal, pkh) (users s)
              }
        , Require $ \input (AddUser u _) ->
            u `notElem` Map.keys (users input)
        ]

data Bid v = Bid
  { newBidder :: User
  , newPkh :: Var PubKeyHash v
  , amt :: Int
  , oldRef :: Var TxOutRef v
  , refundPkh :: Var PubKeyHash v
  , refundAmt :: Int
  }
  deriving stock (Eq, Show, Generic)

instance FunctorB Bid
instance TraversableB Bid

bid :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
bid =
  let gen :: AuctionState Symbolic -> Maybe (m (Bid Symbolic))
      gen s =
        case currentBid s of
          Nothing -> Nothing
          Just (_, currentBidder, amt, _) ->
            Map.toList (users s)
              & filter
                ( \(newBidder, (bal, _)) ->
                    bal > amt && newBidder /= currentBidder
                )
              & \case
                [] -> Nothing
                us -> do
                  (_, u, refundAmt, ref) <- currentBid s
                  (_, refundPkh) <- Map.lookup u (users s)
                  pure $ do
                    (user, (bal, pkh)) <- Gen.element us
                    amt <- Gen.int (Range.linear (amt + 1) bal)
                    pure $ Bid user pkh amt ref refundPkh refundAmt
      execute :: Bid Concrete -> (PropertyT RunIO) TxOutRef
      execute
        ( Bid
            _user
            (Var (Concrete pkh))
            amt
            (Var (Concrete ref))
            (Var (Concrete refundPkh))
            refundAmt
          ) =
          liftRun $ PSM.bid ref pkh amt refundPkh refundAmt
   in Command
        gen
        execute
        [ Update $ \s (Bid newBidder _ newAmt _ _ _) newRef ->
            case currentBid s of
              Nothing -> s
              Just (owner, _, oldAmt, _)
                | oldAmt < newAmt ->
                    s
                      { currentBid = Just (owner, newBidder, newAmt, newRef)
                      }
                | otherwise -> s
        , Require $ \input (Bid {newBidder, amt, refundAmt}) -> isJust $ do
            (_, currentBidder, curAmt, _) <- currentBid input
            bal <- fst <$> Map.lookup newBidder (users input)
            guard $
              currentBidder /= newBidder
                && bal > amt
                && amt > curAmt
                && refundAmt == curAmt
        ]

newtype Start (v :: Type -> Type)
  = Start (User, Var PubKeyHash v)
  deriving stock (Eq, Show, Generic)

instance FunctorB Start
instance TraversableB Start

start :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
start =
  let gen :: AuctionState Symbolic -> Maybe (m (Start Symbolic))
      gen s =
        case filter (enoughForFees . fst . snd) $ Map.toList (users s) of
          [] -> Nothing
          us -> Just $ Start . second snd <$> Gen.element us

      execute :: Start Concrete -> PropertyT RunIO TxOutRef
      execute (Start (_, Var (Concrete pkh))) = liftRun $ PSM.start pkh
   in Command
        gen
        execute
        [ Update $ \s (Start (user, _)) o ->
            s
              { currentBid = Just (user, user, 0, o)
              }
        , Require $ \input (Start (u, _)) ->
            case Map.lookup u (users input) of
              Just (bal, _) -> enoughForFees bal
              Nothing -> False
        , Require $ \input _ ->
            isNothing (currentBid input)
        ]

enoughForFees :: Int -> Bool
enoughForFees = (> 1_000_000)

data End (v :: Type -> Type)
  = End
      (Var PubKeyHash v)
      (Var PubKeyHash v)
      Int
      (Var TxOutRef v)
  deriving stock (Eq, Show, Generic)

instance FunctorB End
instance TraversableB End

end :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
end =
  let gen :: AuctionState Symbolic -> Maybe (m (End Symbolic))
      gen s = case currentBid s of
        Just (owner', won', amt, ref) -> do
          owner <- snd <$> Map.lookup owner' (users s)
          won <- snd <$> Map.lookup won' (users s)
          pure $ pure $ End owner won amt ref
        _ -> Nothing

      execute :: End Concrete -> PropertyT RunIO ()
      execute
        ( End
            (Var (Concrete owner))
            (Var (Concrete winer))
            amt
            (Var (Concrete ref))
          ) = liftRun $ PSM.end owner winer amt ref
   in Command
        gen
        execute
        [ Update $ \s _i _o ->
            s
              { currentBid = Nothing
              , winner = do
                  (_, u, _, _) <- currentBid s
                  pure u
              }
        , Require $ \input (End ownerPkh1 _ _ _) -> isJust $ do
            (ownerU, _, _, _) <- currentBid input
            ownerPkh2 <- snd <$> Map.lookup ownerU (users input)
            guard $ ownerPkh2 == ownerPkh1
        ]

type RunIO = StateT Mock IO

newtype Check (v :: Type -> Type)
  = Check [Var PubKeyHash v]
  deriving stock (Eq, Show, Generic)

instance FunctorB Check
instance TraversableB Check

-- TODO afaict this has to be a command
-- it'd be better if it could be a common callback
-- but the callbacks can't use Run
-- I should try keeping the PSM state in a variable
validate :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
validate =
  let gen :: AuctionState Symbolic -> Maybe (m (Check Symbolic))
      gen s = Just $ pure $ Check (snd <$> Map.elems (users s))

      execute :: Check Concrete -> PropertyT RunIO [(PubKeyHash, Int)]
      execute (Check keys) = liftRun $ PSM.getBals $ concrete <$> keys
   in Command
        gen
        execute
        [ Ensure $ \inp out _ bs -> do
            inp === out
            sort
              [ case currentBid inp of
                Just (_, user, amt, _)
                  | (snd <$> Map.lookup user (users inp))
                      == Just (Var $ Concrete pkh) ->
                      (pkh, bal - amt)
                _ -> (pkh, bal)
              | (bal, Var (Concrete pkh)) <- Map.elems (users out)
              ]
              === sort bs
        ]

auctionTests :: Group
auctionTests =
  Group
    "auction tests"
    [ ("auction test", auctionTest)
    ]

auctionTest :: Property
auctionTest =
  property $ do
    actions <-
      forAll $
        Gen.sequential
          (Range.linear 1 100)
          initialState
          [addUser, bid, start, end, validate]
    execRun mock $ executeSequential initialState actions
  where
    mock = initMock defaultBabbage (adaValue 1_000_000_000_000)

liftRun :: Run a -> PropertyT RunIO a
liftRun (Run act) = do
  let Run checkErrors' = checkErrors
  (res, errs) <- state $ runState (liftM2 (,) act checkErrors')
  case errs of
    Just err -> annotateShow err *> failure
    Nothing -> pure res

execRun :: Mock -> PropertyT RunIO a -> PropertyT IO a
execRun m act = evalStateT (distributeT act) m
