module Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Data.Array (filter, foldl)
import Data.Either (Either(..))
import Data.Hand (readAllHands)
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)


main :: Eff _ _
main = launchAff do
  contents <- readTextFile UTF8 "./poker.txt"
  let splitValues = filter (\x -> S.length x > 0) $ R.split (unsafeRegex "\\s+" global) contents
  case readAllHands splitValues of
    Left error -> log ("Error: " <> error)
    Right allOrderings ->
      let count acc x = if x == GT then acc + 1 else acc
          player1WinCount = (foldl count 0 allOrderings)
      in log $ "Player 1 wins: " <> show player1WinCount <> " times"
