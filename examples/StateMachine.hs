module StateMachine
  (auctionTests
  )
  where

import Hedgehog
    ( forAll,
      property,
      executeSequential,
      FunctorB,
      TraversableB,
      Rec(Rec),
      MonadGen,
      Property,
      Callback(Update),
      Command(Command),
      Concrete,
      Symbolic,
      Var, Group (Group)
    )

import Data.Kind (Type)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Data.Map (Map)
import Data.Map qualified as Map


data AuctionState v
  = AuctionState
    { currentBid :: Maybe (User,Int,Var Utxo v)
    , winner :: Maybe User
    , balances :: Map User Int
    }

initialState :: AuctionState v
initialState =
  AuctionState
    { currentBid = Nothing
    , winner = Nothing
    , balances = Map.fromAscList [(User "a",10),(User "b",20)]
    }

newtype Utxo = Utxo String
  deriving stock (Eq,Ord,Show)
  -- Placeholder type

newtype User = User{name::String}
  deriving stock (Eq,Ord,Show)

data End (v :: Type -> Type) =
  End
  deriving stock (Eq,Show,Generic)

instance FunctorB End
instance TraversableB End

end :: forall n m. (Applicative n,Monad m,MonadIO m) => Command n m AuctionState
end =
  let
    gen :: AuctionState Symbolic -> Maybe (n (End v))
    gen _ = Just $ pure End

    execute :: End Concrete -> m ()
    execute _ = liftIO  $ putStrLn "End"
  in
    Command gen execute
      [ Update $ \s _i _o ->
        s{currentBid = Nothing
         ,winner = currentBid s <&> \(u,_,_) -> u
         }
      ]

data Bid v
  = Bid User Int
  deriving stock (Eq,Show,Generic)

instance FunctorB Bid
instance TraversableB Bid

bid :: forall n m. (Monad n,MonadGen n,Monad m,MonadIO m) => Command n m AuctionState
bid =
  let
    gen :: AuctionState Symbolic -> Maybe (n (Bid v))
    gen s =
      case Map.toList (balances s) of
        [] -> Nothing
        users -> Just $ do
          (user,bal) <- Gen.element users
          Bid
            <$> pure user
            <*> Gen.int (Range.linear 1 bal)
    execute :: Bid Concrete -> m Utxo
    execute (Bid user amt) =
      liftIO $ do
        putStrLn $ "new bid: " <> show user <> " bid " <> show amt
        pure $ Utxo "placeholder"
  in
    Command gen execute
      [ Update $ \s (Bid newBidder newAmt) o ->
        let
          acceptBid = s{currentBid=Just(newBidder,newAmt,o)}
          rejectBid = s
        in
        case currentBid s of
          Nothing ->
            case winner s of
               Just _ -> rejectBid -- auction over
               Nothing -> acceptBid -- fisrt bid
          Just (_,oldAmt,_)
            | oldAmt < newAmt -> acceptBid
            | otherwise -> rejectBid
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
      [bid,end]
    executeSequential initialState actions
