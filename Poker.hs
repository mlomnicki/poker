module Poker where

import Data.List.Split
import Data.List (sort, sortBy, groupBy)
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

isStraight :: HandGrouped -> Maybe HandGrouped
isStraight (hand) =
  case isStraight' flatHand of
    True  -> Just hand
    False -> Nothing
  where flatHand = map rank (concat hand)

isStraight' [Ace, Five, Four, Three, Two] = True
isStraight' flatHand =
  let pairs = zip flatHand (drop 1 flatHand) in
    all (\(x,y) -> ((maxBound :: Rank) /= y) && (succ y == x)) pairs

isFlush :: HandGrouped -> Maybe HandGrouped
isFlush (hand) =
  case all (== x) xs of
    True  -> Just hand
    False -> Nothing
  where (x:xs) = map suit (concat hand)

isFullHouse :: HandGrouped -> Maybe HandGrouped
isFullHouse hand =
  case rest of
    Nothing -> Nothing
    Just rest -> isPair rest
  where rest = isSet hand

isQuads :: HandGrouped -> Maybe HandGrouped
isQuads ([_,_,_,_]:xs) = (Just xs)
isQuads _ = Nothing

isStraightFlush :: HandGrouped -> Maybe HandGrouped
isStraightFlush hand =
  case isStraight hand of
    Just hand -> isFlush hand
    Nothing   -> Nothing

isRoyalFlush :: HandGrouped -> Maybe HandGrouped
isRoyalFlush hand =
  case isFlush hand of
    Just hand ->
      if flatHand == [Ace, King, Queen, Jack, Ten]
      then Just hand
      else Nothing
    Nothing   -> Nothing
  where flatHand = map rank (concat hand)

rank :: Card -> Rank
rank card = (fst card)

suit :: Card -> Suit
suit card = (snd card)

makeHighestCard   [[a], _, _, _, _]                 = HighestCard   (rank a)
makePair          [[a, _], _ , _, _]                = Pair          (rank a)
makeTwoPairs      [[a, _], [b, _], _]               = TwoPairs      (rank a) (rank b)
makeSet           [[a, _, _], _, _]                 = Set           (rank a)
makeStraight      [[(Ace, _)], _, _, _, [(Two, _)]] = Straight      Ace
makeStraight      [_, _, _, _, [a]]                 = Straight      (rank a)
makeFlush         [[a], _, _, _, _]                 = Flush         (rank a)
makeFullHouse     [[a, _, _], [b, _]]               = FullHouse     (rank a) (rank b)
makeQuads         [[a, _, _, _], [_]]               = Quads         (rank a)
makeStraightFlush [[(Ace, _)], _, _, _, [(Two, _)]] = StraightFlush Ace
makeStraightFlush [_, _, _, _, [a]]                 = StraightFlush (rank a)

handType :: Hand -> HandType
handType hand = handType' (groupHand hand)

handType' hand
  | (isJust (isRoyalFlush    hand)) = RoyalFlush
  | (isJust (isStraightFlush hand)) = makeStraightFlush hand
  | (isJust (isQuads         hand)) = makeQuads         hand
  | (isJust (isFullHouse     hand)) = makeFullHouse     hand
  | (isJust (isFlush         hand)) = makeFlush         hand
  | (isJust (isStraight      hand)) = makeStraight      hand
  | (isJust (isSet           hand)) = makeSet           hand
  | (isJust (isTwoPairs      hand)) = makeTwoPairs      hand
  | (isJust (isPair          hand)) = makePair          hand
  | otherwise                       = makeHighestCard   hand
