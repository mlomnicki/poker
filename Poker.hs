module Poker where

import Data.List.Split
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (isJust)

data Rank = Two
     | Three
     | Four
     | Five
     | Six
     | Seven
     | Eight
     | Nine
     | Ten
     | Jack
     | Queen
     | King
     | Ace
     deriving (Read, Show, Enum, Eq, Ord, Bounded)

data Suit = Clubs | Diamonds | Spades | Hearts
    deriving (Read, Show, Enum, Eq)

type Card = (Rank, Suit)
type Deck = [Card]
type Hand = [Card]

data HandType = HighestCard   Rank
              | Pair          Rank
              | TwoPairs      Rank Rank
              | Set           Rank
              | Straight      Rank
              | Flush         Rank
              | FullHouse     Rank Rank
              | Quads         Rank
              | StraightFlush Rank
              | RoyalFlush
              deriving (Show, Eq)

type HandGrouped = [[Card]]

groupHand hand = reverse $
                 sortBy (comparing length) $
                 groupBy ((==) `on` fst) $
                 sortBy (comparing fst) hand


isPair :: HandGrouped -> Maybe HandGrouped
isPair ([_,_]:xs) = (Just xs)
isPair _ = Nothing

isTwoPairs :: HandGrouped -> Maybe HandGrouped
isTwoPairs hand =
   case rest of
     Nothing -> Nothing
     Just rest -> isPair rest
    where rest = isPair hand

isSet :: HandGrouped -> Maybe HandGrouped
isSet ([_,_,_]:xs) = (Just xs)
isSet _ = Nothing

isFullHouse :: HandGrouped -> Maybe HandGrouped
isFullHouse hand =
  case rest of
    Nothing -> Nothing
    Just rest -> isPair rest
  where rest = isSet hand

rank :: Card -> Rank
rank card = (fst card)

makeFullHouse [[a, _, _], [b, _]] = FullHouse (rank a) (rank b)
makePair      [[a, _], _ , _, _]  = Pair      (rank a)
makeTwoPairs  [[a, _], [b, _], _] = TwoPairs  (rank a) (rank b)
makeSet       [[a, _, _], _, _]   = Set       (rank a)

handType :: Hand -> HandType
handType hand = handType' (groupHand hand)

handType' hand
  | (isJust (isFullHouse hand)) = makeFullHouse hand
  | (isJust (isSet       hand)) = makeSet       hand
  | (isJust (isTwoPairs  hand)) = makeTwoPairs  hand
  | (isJust (isPair      hand)) = makePair      hand
  | otherwise                   = error "unknown hand"
