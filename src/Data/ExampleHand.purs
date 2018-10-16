module Data.ExampleHand where

import Data.Hand
import Data.Card (Rank(..), Suit(..), card)

exampleHighCard = hand (card Five Clubs) (card Seven Diamonds)
  (card King Hearts) (card Two Spades) (card Ace Clubs)

examplePair = hand (card Five Clubs) (card Five Clubs)
  (card King Hearts) (card Two Spades) (card Ace Clubs)

exampleTwoPair = hand (card King Clubs) (card Five Clubs)
  (card King Hearts) (card Five Spades) (card Ace Clubs)

exampleThreeOfAKind = hand (card Ten Clubs) (card Ten Spades)
  (card Queen Spades) (card Two Spades) (card Ten Hearts)

exampleStraight = hand (card Two Clubs) (card Three Clubs)
  (card Five Hearts) (card Four Spades) (card Six Diamonds)

exampleFlush = hand (card Jack Clubs) (card Six Clubs)
  (card Queen Clubs) (card Two Clubs) (card Ten Clubs)

exampleFullHouse = hand (card Jack Clubs) (card Jack Spades)
  (card Jack Diamonds) (card Two Clubs) (card Two Hearts)

exampleFourOfAKind = hand (card Three Clubs) (card Three Hearts)
  (card Queen Clubs) (card Three Diamonds) (card Three Spades)

exampleStraightFlush = hand (card Jack Clubs) (card Ten Clubs)
  (card Queen Clubs) (card Nine Clubs) (card Eight Clubs)
