module Auction.StateMachine
  (auctionTests
  )
  where

import Hedgehog
    ( forAll,
      property,
      executeSequential,
      MonadGen,
      Property,
      Callback(Update),
      Command(Command),
      Concrete(Concrete),
      Symbolic,
      Group (Group),
      PropertyT,
      FunctorB,
      TraversableB,
      Rec(Rec),
      Var(Var)

    )
import Hedgehog.Internal.Distributive(MonadTransDistributive(distributeT))

import Control.Monad.State.Strict (StateT,state,runState, evalStateT)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map (Map)
import GHC.Generics (Generic)
import Plutus.Model (Mock,Run(Run),initMock, adaValue, defaultBabbage,checkErrors)
import PlutusLedgerApi.V2 (PubKeyHash)

import qualified Data.Map as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import qualified Auction.PSM as PSM

newtype User = User{name::String}
  deriving stock (Eq,Ord,Show)

data AuctionState v
  = AuctionState
    { currentBid :: Maybe (User,Int)
    , winner :: Maybe User
    , balances :: Map User Int
    , userKeys :: Map User (Var PubKeyHash v)
    }

initialState :: AuctionState v
initialState =
  AuctionState
    { currentBid = Nothing
    , winner = Nothing
    , balances = Map.empty
    , userKeys = Map.empty
    }

data AddUser v
  = AddUser User Int
  deriving stock (Eq,Show,Generic)

instance FunctorB AddUser
instance TraversableB AddUser

addUser :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
addUser =
  let
    gen :: AuctionState Symbolic -> Maybe (m (AddUser v))
    gen s = Just $ AddUser <$>
      Gen.filterT (\user -> user `notElem` (Map.keys $ userKeys s))
      (User <$> Gen.list (Range.linear 0 100) Gen.alpha)
      <*> Gen.int (Range.linear 0 1_000_000_000)

    execute :: AddUser Concrete -> PropertyT RunIO PubKeyHash
    execute (AddUser _user startBal) = liftRun $ PSM.addUser startBal
  in
    Command gen execute
      [ Update $ \s (AddUser user startBal) pkh ->
        s{userKeys = Map.insert user pkh (userKeys s)
         ,balances = Map.insert user startBal (balances s)
         }
      ]

data Bid v
  = Bid User (Var PubKeyHash v) Int
  deriving stock (Eq,Show,Generic)

instance FunctorB Bid
instance TraversableB Bid

bid :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
bid =
  let
    gen :: AuctionState Symbolic -> Maybe (m (Bid Symbolic))
    gen s =
      case Map.toList (balances s) of
        [] -> Nothing
        users -> Just $ do
          (user,bal) <- Gen.element users
          pkh <- case Map.lookup user (userKeys s) of
                   Just pkh -> pure pkh
                   Nothing -> error "no key for user"
          Bid user pkh <$>
            Gen.int (Range.linear 1 bal)
    execute :: Bid Concrete -> (PropertyT RunIO) ()
    execute (Bid _user (Var (Concrete pkh)) amt) = liftRun $ PSM.bid pkh amt
  in
    Command gen execute
      [ Update $ \s (Bid newBidder _pkh newAmt) _ ->
        let
          acceptBid = s{currentBid=Just(newBidder,newAmt)}
          rejectBid = s
        in
        case currentBid s of
          Nothing ->
            case winner s of
               Just _ -> rejectBid -- auction over
               Nothing -> acceptBid -- fisrt bid
          Just (_,oldAmt)
            | oldAmt < newAmt -> acceptBid
            | otherwise -> rejectBid
      ]

data End (v :: Type -> Type) =
  End
  deriving stock (Eq,Show,Generic)

instance FunctorB End
instance TraversableB End

end :: forall m. MonadGen m => Command m (PropertyT RunIO) AuctionState
end =
  let
    gen :: AuctionState Symbolic -> Maybe (m (End v))
    gen _ = Just $ pure End

    execute :: End Concrete -> PropertyT RunIO ()
    execute _ = liftRun PSM.end
  in
    Command gen execute
      [ Update $ \s _i _o ->
        s{currentBid = Nothing
         ,winner = currentBid s <&> fst
         }
      ]

auctionTests :: Group
auctionTests = Group "auction tests" [("auction test",auctionTest)]

auctionTest :: Property
auctionTest =
  property $ do
    actions <- forAll $
      Gen.sequential
      (Range.linear 1 100)
      initialState
      [addUser,bid,end]
    execRun mock $ executeSequential initialState actions
      where
        mock = initMock defaultBabbage (adaValue 1_000_000_000)

type RunIO = StateT Mock IO

liftRun :: Run a -> PropertyT RunIO a
liftRun (Run act) = state $ runState act

execRun :: Mock -> PropertyT RunIO a -> PropertyT IO a
execRun m act = evalStateT (distributeT (act <* liftRun checkErrors)) m
