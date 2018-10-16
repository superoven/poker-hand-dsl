module Data.Card where

import Prelude

import Data.Enum (class Enum)
import Data.Maybe (Maybe(..))

data Rank
  = Two
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
derive instance eqRank :: Eq Rank
derive instance ordRank :: Ord Rank
instance enumRank :: Enum Rank where
  succ a = case a of
    Two   -> Just Three
    Three -> Just Four
    Four  -> Just Five
    Five  -> Just Six
    Six   -> Just Seven
    Seven -> Just Eight
    Eight -> Just Nine
    Nine  -> Just Ten
    Ten   -> Just Jack
    Jack  -> Just Queen
    Queen -> Just King
    King  -> Just Ace
    Ace   -> Nothing
  pred a = case a of
    Ace   -> Just King
    King  -> Just Queen
    Queen -> Just Jack
    Jack  -> Just Ten
    Ten   -> Just Nine
    Nine  -> Just Eight
    Eight -> Just Seven
    Seven -> Just Six
    Six   -> Just Five
    Five  -> Just Four
    Four  -> Just Three
    Three -> Just Two
    Two   -> Nothing -- Purescript really should be able to derive this
instance showRank :: Show Rank where
  show x = case x of
    Two   -> "2"
    Three -> "3"
    Four  -> "4"
    Five  -> "5"
    Six   -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine  -> "9"
    Ten   -> "10"
    Jack  -> "J"
    Queen -> "Q"
    King  -> "K"
    Ace   -> "A"

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
derive instance eqSuit :: Eq Suit
derive instance ordSuit :: Ord Suit
instance showSuit :: Show Suit where
  show x = case x of
    Clubs    -> "♧ "
    Diamonds -> "♢ "
    Hearts   -> "♡ "
    Spades   -> "♤ "

data Card = Card
  { rank :: Rank
  , suit :: Suit }
derive instance eqCard :: Eq Card
instance ordCard :: Ord Card where
  compare = ordByRank
instance showCard :: Show Card where
  show (Card card) = show card.rank <> show card.suit
eqByRank :: Card -> Card -> Boolean
eqByRank (Card a) (Card b) = eq a.rank b.rank
eqBySuit :: Card -> Card -> Boolean
eqBySuit (Card a) (Card b) = eq a.suit b.suit
ordByRank :: Card -> Card -> Ordering
ordByRank (Card a) (Card b) = compare a.rank b.rank
ordBySuit :: Card -> Card -> Ordering
ordBySuit (Card a) (Card b) = compare a.suit b.suit

card :: Rank -> Suit -> Card
card rank suit = Card { rank, suit }
