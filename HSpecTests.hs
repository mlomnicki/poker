module Main where

import Poker
import Test.Hspec
import Data.List.Split

mkCard :: String -> Card
mkCard str = let rank = case (head str) of
                         'A' -> Ace
                         'K' -> King
                         'Q' -> Queen
                         'J' -> Jack
                         'T' -> Ten
                         n   -> toEnum (read [n] - 2)

                 suit = case (tail str) of
                         "s" -> Spades
                         "d" -> Diamonds
                         "c" -> Clubs
                         "h" -> Hearts
             in (rank, suit)

mkHand :: String -> Hand
mkHand cards = map mkCard (splitOn " " cards)

main :: IO ()
main = hspec $ do

  it "one pair" $ do
    handType (mkHand "As 6s 2d Ad Kh") `shouldBe` Pair Ace
    handType (mkHand "Kd 2c 7h 2d Ad") `shouldBe` Pair Two

  it "two pairs" $ do
    handType (mkHand "As 2c 2d Ad Kh") `shouldBe` TwoPairs Ace Two
    handType (mkHand "3s 3s Kh 2d Kd") `shouldBe` TwoPairs King Three

  it "set" $ do
    handType (mkHand "As Kc 2d Kd Kh") `shouldBe` Set King
    handType (mkHand "3s 3c Kh 3d Td") `shouldBe` Set Three

  it "straight" $ do
    handType (mkHand "3s 4s 5d 6h 7s") `shouldBe` Straight Three
    handType (mkHand "As 2s 3d 4h 5s") `shouldBe` Straight Ace
    handType (mkHand "3s 4d 5d 6d 2h") `shouldBe` Straight Two
    handType (mkHand "Ts Jd Qd Kd Ah") `shouldBe` Straight Ten
    handType (mkHand "Jd Qd Kd Ah 2d") `shouldBe` HighestCard Ace

  it "flush" $ do
    handType (mkHand "2s 5s 3s As 6s") `shouldBe` Flush Ace
    handType (mkHand "Qd 2d Jd 7d Td") `shouldBe` Flush Queen

  it "full house" $ do
    handType (mkHand "6d 6c 6h 2d 2s") `shouldBe` FullHouse Six Two
    handType (mkHand "2d 6d 2c 6c 6h") `shouldBe` FullHouse Six Two
    handType (mkHand "Ad 6d Ac 6h As") `shouldBe` FullHouse Ace Six

  it "quads" $ do
    handType (mkHand "6d 6c 6h 6s 2s") `shouldBe` Quads Six
    handType (mkHand "Ad 6c Ah As As") `shouldBe` Quads Ace

  it "straight flush" $ do
    handType (mkHand "3s 4s 5s 6s 7s") `shouldBe` StraightFlush Three
    handType (mkHand "Ad 2d 3d 4d 5d") `shouldBe` StraightFlush Ace
    handType (mkHand "3h 4h 5h 6h 2h") `shouldBe` StraightFlush Two

  it "royal flush" $ do
    handType (mkHand "Ts Js Qs Ks As")  `shouldBe` RoyalFlush
    handType (mkHand "Ad Jd Kd Td Qd") `shouldBe` RoyalFlush
