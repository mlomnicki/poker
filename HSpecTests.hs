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

  it "full house" $ do
    handType (mkHand "6d 6c 6h 2d 2s") `shouldBe` FullHouse Six Two
    handType (mkHand "2d 6d 2c 6c 6h") `shouldBe` FullHouse Six Two
    handType (mkHand "Ad 6d Ac 6h As") `shouldBe` FullHouse Ace Six
