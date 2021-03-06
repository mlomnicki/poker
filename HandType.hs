module HandType where

import Data.List.Split
import Data.List (sort, sortBy, groupBy, maximum)
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
                 groupBy ((==) `on` rank) $
                 sortBy (comparing rank) hand


isPair :: Hand -> Maybe HandType
isPair hand         = isPair' (groupHand hand)
isPair' ([a, _]:xs) = Just (Pair (rank a))
isPair' _           = Nothing

isTwoPairs :: Hand -> Maybe HandType
isTwoPairs  hand               = isTwoPairs' (groupHand hand)
isTwoPairs' ([a, _]:[b, _]:xs) = Just (TwoPairs (rank a) (rank b))
isTwoPairs' _                  = Nothing

isSet :: Hand -> Maybe HandType
isSet  hand           = isSet' (groupHand hand)
isSet' ([a, _, _]:xs) = Just (Set (rank a))
isSet' _              = Nothing

isStraight hand
  | h == [Two   .. Six]                = Just (Straight Two)
  | h == [Three .. Seven]              = Just (Straight Three)
  | h == [Four  .. Eight]              = Just (Straight Four)
  | h == [Five  .. Nine]               = Just (Straight Five)
  | h == [Six   .. Ten]                = Just (Straight Six)
  | h == [Seven .. Jack]               = Just (Straight Seven)
  | h == [Eight .. Queen]              = Just (Straight Eight)
  | h == [Nine  .. King]               = Just (Straight Nine)
  | h == [Ten   .. Ace]                = Just (Straight Ten)
  | h == [Two   .. Five] ++ [Ace]      = Just (Straight Ace)
  | otherwise                          = Nothing
  where h = sort (map rank hand)

isFlush :: Hand -> Maybe HandType
isFlush hand
  | (length sameSuitCards) >= 5 = Just (Flush $ maximum $ map rank sameSuitCards)
  | otherwise = Nothing
  where (sameSuitCards:_) = sortBy (comparing (negate . length)) $
                            groupBy ((==) `on` suit) hand

isFullHouse :: Hand -> Maybe HandType
isFullHouse hand                   = isFullHouse' (groupHand hand)
isFullHouse' ([a, _, _]:[b, _]:xs) = Just (FullHouse (rank a) (rank b))
isFullHouse' _                     = Nothing

isQuads :: Hand -> Maybe HandType
isQuads hand               = isQuads' (groupHand hand)
isQuads' ([a, _, _, _]:xs) = Just (Quads (rank a))
isQuads' _                 = Nothing

isStraightFlush :: Hand -> Maybe HandType
isStraightFlush hand =
  isFlush hand >> isStraight hand >>= \(Straight t) -> Just (StraightFlush t)

isRoyalFlush :: Hand -> Maybe HandType
isRoyalFlush hand
  | Just (StraightFlush Ten) == (isStraightFlush hand) = Just RoyalFlush
  | otherwise = Nothing

tryHand hand [] = HighestCard $ maximum $ map rank hand

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
