module Test.HandRank where

import Prelude

import Data.Card (Rank(Queen, Three, Two, Jack, Six, Ten, Ace, Five, King, Four, Seven))
import Data.Hand (HandRank(StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPair, Pair, HighCard), getHandRank)
import Data.ExampleHand (exampleFlush, exampleFourOfAKind, exampleFullHouse, exampleHighCard, examplePair, exampleStraight, exampleStraightFlush, exampleThreeOfAKind, exampleTwoPair)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

handRankTestSpec :: ∀ e. Spec (RunnerEffects e) Unit
handRankTestSpec =
  describe "Rank Checking" do
    describe "High Card" do
      it "Parses a basic High Card" $
        (getHandRank exampleHighCard) `shouldEqual` HighCard Ace King Seven Five Two
      describe "Hierarchy" do
        it "Is worse than Pair" $
          (HighCard King Queen Ten Jack Two > Pair Two King Queen Jack) `shouldEqual` false
        it "Is worse than Flush" $
          (HighCard King Queen Ten Jack Two < Flush) `shouldEqual` true
        it "Is worse than Straight Flush" $
          (HighCard King Queen Ten Jack Two < StraightFlush Four) `shouldEqual` true
    describe "Pair" do
      it "Parses a basic Pair" $
        (getHandRank examplePair) `shouldEqual` Pair Five Ace King Two
    describe "Two Pair" do
      it "Parses a basic Two Pair" $
        (getHandRank exampleTwoPair) `shouldEqual` TwoPair King Five Ace
    describe "Three of a Kind" do
      it "Parses a basic Three of a Kind" $
        (getHandRank exampleThreeOfAKind) `shouldEqual` ThreeOfAKind Ten Queen Two
    describe "Straight" do
      it "Parses a basic Straight" $
        (getHandRank exampleStraight) `shouldEqual` Straight Six
    describe "Flush" do
      it "Parses a basic Three of a Kind" $
        (getHandRank exampleFlush) `shouldEqual` Flush
    describe "Full House" do
      it "Parses a basic Full House" $
        (getHandRank exampleFullHouse) `shouldEqual` FullHouse Jack Two
    describe "Four of a Kind" do
      it "Parses a basic Four of a Kind" $
        (getHandRank exampleFourOfAKind) `shouldEqual` FourOfAKind Three Queen
    describe "Straight Flush" do
      it "Parses a basic Straight Flush" $
        (getHandRank exampleStraightFlush) `shouldEqual` StraightFlush Queen
