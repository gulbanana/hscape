module Mob where

data AIState = NoWander | WanderRight | WanderLeft
  deriving Eq

data Mob = Mob {
  name :: String,
  sym :: Char,
  x :: Int,
  y :: Int,
  state :: AIState
} deriving Eq