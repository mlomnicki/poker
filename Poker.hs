module Poker where

import Data.List.Split
import Data.List (sort, sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)

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

rank :: Card -> Rank
rank card = (fst card)

suit :: Card -> Suit
suit card = (snd card)

groupHand hand = reverse $
                 sortBy (comparing length) $
                 groupBy ((==) `on` fst) $
                 sortBy (comparing fst) hand


isPair :: Hand -> Maybe HandType
isPair hand =
  case groupHand hand of
    ([a, _]:xs) -> Just (Pair (rank a))
    otherwise   -> Nothing

isTwoPairs :: Hand -> Maybe HandType
isTwoPairs hand =
  case groupHand hand of
    ([a, _]:[b, _]:xs) -> Just (TwoPairs (rank a) (rank b))
    otherwise          -> Nothing

isSet :: Hand -> Maybe HandType
isSet hand =
  case groupHand hand of
    ([a, _, _]:xs) -> Just (Set (rank a))
    otherwise      -> Nothing

isStraight :: Hand -> Maybe HandType
isStraight (hand) =
  case sort (map rank hand) of
    [Two, Three, Four, Five, Ace]   -> Just (Straight Ace)
    [Two, Three, Four, Five, Six]   -> Just (Straight Two)
    [Three, Four, Five, Six, Seven] -> Just (Straight Three)
    [Four, Five, Six, Seven, Eight] -> Just (Straight Four)
    [Five, Six, Seven, Eight, Nine] -> Just (Straight Five)
    [Six, Seven, Eight, Nine, Ten]  -> Just (Straight Six)
    [Seven, Eight, Nine, Ten, Jack] -> Just (Straight Seven)
    [Eight, Nine, Ten, Jack, Queen] -> Just (Straight Eight)
    [Nine, Ten, Jack, Queen, King]  -> Just (Straight Nine)
    [Ten, Jack, Queen, King, Ace]   -> Just (Straight Ten)
    otherwise -> Nothing

isFlush :: Hand -> Maybe HandType
isFlush hand =
  isFlush' (head (reverse (sortBy (comparing length) (groupBy ((==) `on` suit) hand))))

isFlush' handG =
  if (length handG) >= 5
    then Just (Flush (last (sort (map rank handG))))
    else Nothing

isFullHouse :: Hand -> Maybe HandType
isFullHouse hand =
  case groupHand hand of
    ([a, _, _]:[b, _]:xs) -> Just (FullHouse (rank a) (rank b))
    otherwise             -> Nothing

isQuads :: Hand -> Maybe HandType
isQuads hand =
  case groupHand hand of
    ([a, _, _, _]:xs) -> Just (Quads (rank a))
    otherwise         -> Nothing

isStraightFlush :: Hand -> Maybe HandType
isStraightFlush hand =
  isFlush hand >> isStraight hand >>= \(Straight t) -> Just (StraightFlush t)

isRoyalFlush :: Hand -> Maybe HandType
isRoyalFlush hand =
  case isStraightFlush hand of
    Just (StraightFlush Ten) -> Just RoyalFlush
    otherwise                -> Nothing

tryHand hand [] =
  HighestCard (head (reverse (sort (map rank hand))))

tryHand hand (f:fs) =
  case f hand of
    Just handType -> handType
    Nothing       -> tryHand hand fs

handType :: Hand -> HandType
handType hand =
  tryHand hand [
    isRoyalFlush,
    isStraightFlush,
    isQuads,
    isFullHouse,
    isFlush,
    isStraight,
    isSet,
    isTwoPairs,
    isPair
  ]
