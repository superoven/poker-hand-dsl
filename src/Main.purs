module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (take)
import Data.Deck (deck, shuffle)

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit
main = do
  log "Hello world"
