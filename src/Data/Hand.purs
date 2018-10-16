module Data.Hand where

import Prelude

import Data.NonEmpty (fromNonEmpty, NonEmpty)
import Control.Alt ((<|>))
import Data.Array (all, groupBy, length, snoc, sortBy, tail, zip)
import Data.Card (Card(Card), Rank(Two), Suit, eqByRank, eqBySuit, ordBySuit)
import Data.Enum (pred)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

newtype Hand = Hand (Array Card)
instance showHand :: Show Hand where
  show orig@(Hand xs) = show xs <> ", " <> show (getHandRank orig)
instance eqHand :: Eq Hand where
  eq a b = eq (getHandRank a) (getHandRank b)
instance ordHand :: Ord Hand where
  compare a b = compare (getHandRank a) (getHandRank b)

hand :: Card -> Card -> Card -> Card -> Card -> Hand
hand c1 c2 c3 c4 c5 = Hand $ sortBy (flip compare) [c1, c2, c3, c4, c5]

toArray :: NonEmpty Array Card -> Array Card
toArray = fromNonEmpty (flip snoc)

groupRank :: Hand -> Array (Array Card)
groupRank (Hand xs) = sortGroup $ map toArray $ groupBy eqByRank xs
groupSuit :: Hand -> Array (Array Card)
groupSuit (Hand xs) = sortGroup $ map toArray $ groupBy eqBySuit (sortBy ordBySuit xs)
sortGroup :: Array (Array Card) -> Array (Array Card)
sortGroup = sortBy (\xs ys -> (flip compare) (length xs) (length ys))

type Kicker = Rank

data HandRank
  = HighCard Rank Kicker Kicker Kicker Kicker
  | Pair Rank Kicker Kicker Kicker
  | TwoPair Rank Rank Kicker
  | ThreeOfAKind Rank Kicker Kicker
  | Straight Rank
  | Flush
  | FullHouse Rank Rank
  | FourOfAKind Rank Kicker
  | StraightFlush Rank
derive instance eqHandRank :: Eq HandRank
derive instance ordHandRank :: Ord HandRank
instance showHandRank :: Show HandRank where
  show x = case x of
    (HighCard r k1 _ _ _) ->
      "High Card: " <> show r <> ", " <> show k1 <> " high"
    (Pair r k1 _ _) ->
      "Pair: " <> show r <> "'s " <> show k1 <> " high"
    (TwoPair r1 r2 k) ->
      "Two Pairs: " <> show r1 <> " and " <> show r2 <> ", " <> show k <> " high"
    (ThreeOfAKind r k1 _) ->
      "Three of a Kind: " <> show r <> ", " <> show k1 <> " high"
    (Straight r) ->
      "Straight: " <> show r <> " high"
    (Flush) ->
      "Flush"
    (FullHouse r1 r2) ->
      "Full House: " <> show r1 <> " and " <> show r2
    (FourOfAKind r k) ->
      "Four of a Kind: " <> show r <> ", " <> show k <> " high"
    (StraightFlush r) ->
      "Straight Flush: " <> show r <> " high"

getHandRank :: Hand -> HandRank
getHandRank currHand = fromMaybe (getHighCard groupedByRank)
  $   isStraightFlush currHand
  <|> isFourOfAKind groupedByRank
  <|> isFlush groupedBySuit
  <|> isFullHouse groupedByRank
  <|> isStraight currHand
  <|> isThreeOfAKind groupedByRank
  <|> isTwoPair groupedByRank
  <|> isPair groupedByRank
  where
    groupedBySuit = groupSuit currHand
    groupedByRank = groupRank currHand

isStraightFlush :: Hand -> Maybe HandRank
isStraightFlush currHand@(Hand [(Card firstCard), _, _, _, _]) = do
  x <- isStraight currHand
  y <- isFlush groupedBySuit
  pure $ StraightFlush firstCard.rank
  where groupedBySuit = groupSuit currHand
isStraightFlush _ = Nothing

isFourOfAKind :: Array (Array Card) -> Maybe HandRank
isFourOfAKind [[(Card r), _, _, _], [(Card k)]] = Just $ FourOfAKind r.rank k.rank
isFourOfAKind _ = Nothing

isFullHouse :: Array (Array Card) -> Maybe HandRank
isFullHouse [[(Card r1), _, _], [(Card r2), _]] = Just $ FullHouse r1.rank r2.rank
isFullHouse _ = Nothing

isFlush :: Array (Array Card) -> Maybe HandRank
isFlush [[(Card s), _, _, _, _]] = Just $ Flush
isFlush _ = Nothing

isStraight :: Hand -> Maybe HandRank
isStraight (Hand cards@[(Card a), _, _, _, _]) =
  if isConsecutive cards
  then Just (Straight a.rank)
  else Nothing
  where
    pairs xs = case tail xs of
      Nothing -> []
      Just t  -> zip xs t
    checkFunc (Tuple (Card a) (Card b)) = pred a.rank == Just b.rank
    isConsecutive xs = all checkFunc (pairs xs)
isStraight _ = Nothing

isThreeOfAKind :: Array (Array Card) -> Maybe HandRank
isThreeOfAKind [[(Card r), _, _], [(Card k1)], [(Card k2)]]
  = Just $ ThreeOfAKind r.rank k1.rank k2.rank
isThreeOfAKind _ = Nothing

isTwoPair :: Array (Array Card) -> Maybe HandRank
isTwoPair [[(Card pair1), _], [(Card pair2), _], [(Card k)]]
  = Just $ TwoPair pair1.rank pair2.rank k.rank
isTwoPair _ = Nothing

isPair :: Array (Array Card) -> Maybe HandRank
isPair [[(Card pair), _], [(Card k1)], [(Card k2)], [(Card k3)]]
  = Just $ Pair pair.rank k1.rank k2.rank k3.rank
isPair _ = Nothing

getHighCard :: Array (Array Card) -> HandRank
getHighCard [[(Card h)], [(Card k1)], [(Card k2)], [(Card k3)], [(Card k4)]]
  = HighCard h.rank k1.rank k2.rank k3.rank k4.rank
getHighCard _ = HighCard Two Two Two Two Two -- This is illegal and will be tested against
