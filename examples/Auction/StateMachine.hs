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
import Control.Monad (liftM2)
import Control.Monad.State.Strict (StateT, evalStateT, runState, state)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List (sort)
import Data.Map (Map)
import GHC.Generics (Generic)
import Plutus.Model (Mock, Run (Run), adaValue, checkErrors, defaultBabbage, initMock)
import PlutusLedgerApi.V2 (PubKeyHash)

import Auction.PSM qualified as PSM
import Data.Map qualified as Map
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range qualified as Range
import PlutusLedgerApi.V2.Contexts (TxOutRef)

newtype User = User {name :: String}
  deriving stock (Eq, Ord, Show)

data AuctionState v = AuctionState
  { currentBid :: Maybe (User, Int)
  , winner :: Maybe User
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
              (\user -> user `notElem` (Map.keys $ users s))
              (User <$> Gen.list (Range.linear 0 100) Gen.alpha)
            <*> Gen.int (Range.linear 0 1_000_000_000)

      execute :: AddUser Concrete -> PropertyT RunIO PubKeyHash
      execute (AddUser _user startBal) = liftRun $ PSM.addUser startBal
   in Command
        gen
        execute
        [ Update $ \s (AddUser user startBal) pkh ->
            s
              { users = Map.insert user (startBal, pkh) (users s)
              }
        , Require $ \input (AddUser u _) -> u `notElem` (Map.keys $ users input)
        ]

data Bid v
  = Bid User (Var PubKeyHash v) Int
  deriving stock (Eq, Show, Generic)

instance FunctorB Bid
instance TraversableB Bid

bid :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
bid =
  let gen :: AuctionState Symbolic -> Maybe (m (Bid Symbolic))
      gen s =
        case Map.toList (users s) of
          [] -> Nothing
          us -> Just $ do
            (user, (bal, pkh)) <- Gen.element us
            Bid user pkh
              <$> Gen.int (Range.linear 1 bal)
      execute :: Bid Concrete -> (PropertyT RunIO) ()
      execute (Bid _user (Var (Concrete pkh)) amt) = liftRun $ PSM.bid pkh amt
   in Command
        gen
        execute
        [ Update $ \s (Bid newBidder _pkh newAmt) _ ->
            let
              acceptBid =
                s
                  { currentBid =
                      Just (newBidder, newAmt)
                  }
              rejectBid = s
             in
              case currentBid s of
                Nothing ->
                  case winner s of
                    Just _ -> rejectBid -- auction over
                    Nothing -> acceptBid -- fisrt bid
                Just (_, oldAmt)
                  | oldAmt < newAmt -> acceptBid
                  | otherwise -> rejectBid
        ]

data Start (v :: Type -> Type)
  = Start (User, Var PubKeyHash v)
  deriving stock (Eq, Show, Generic)

instance FunctorB Start
instance TraversableB Start

start :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
start =
  let gen :: AuctionState Symbolic -> Maybe (m (Start Symbolic))
      gen s =
        case filter ((> 1) . fst . snd) $ Map.toList (users s) of
          [] -> Nothing
          us -> Just $ Start . second snd <$> Gen.element us

      execute :: Start Concrete -> PropertyT RunIO TxOutRef
      execute (Start (_, Var (Concrete pkh))) = liftRun $ PSM.start pkh
   in Command
        gen
        execute
        [ Update $ \s (Start (user, _)) _o ->
            s
              { currentBid = Just (user, 0)
              }
        , Require $ \input (Start (u, _)) ->
            case Map.lookup u (users input) of
              Just (bal, _) -> bal > 1
              Nothing -> False
        ]

data End (v :: Type -> Type)
  = End
  deriving stock (Eq, Show, Generic)

instance FunctorB End
instance TraversableB End

end :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
end =
  let gen :: AuctionState Symbolic -> Maybe (m (End v))
      gen _ = Just $ pure End

      execute :: End Concrete -> PropertyT RunIO ()
      execute _ = liftRun PSM.end
   in Command
        gen
        execute
        [ Update $ \s _i _o ->
            s
              { currentBid = Nothing
              , winner = currentBid s <&> fst
              }
        ]

type RunIO = StateT Mock IO

data Check (v :: Type -> Type)
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
            sort [(pkh, bal) | (bal, (Var (Concrete pkh))) <- Map.elems (users out)]
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
