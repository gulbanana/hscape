{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game (initModel, updateModel, Mob(..), Model(..), Action(..)) where

import Data.List
import Miso
import Miso.String (MisoString, concat)

data AIState = NoWander | WanderRight | WanderLeft
  deriving Eq

data Mob = Mob {
  name :: MisoString,
  sym :: MisoString,
  x :: Int,
  y :: Int,
  state :: AIState
} deriving Eq

data Model = Model {
  player :: Mob,
  enemies :: [Mob],
  logLines :: [MisoString]
} deriving Eq

data Action = NoOp
            | Init
            | MoveDelta Int Int
            | Wait
            | Log MisoString

data Command = MoveTo Mob (Model -> Mob -> Model) Int Int
             | Attack Mob Mob
             | ChangeDirection Mob (Model -> Mob -> Model)

initModel :: Model
initModel = Model (Mob "player" "@" 40 12 NoWander) [Mob "dog" "d" 1 21 WanderRight, Mob "wolf" "d" 4 6 WanderLeft, Mob "canine" "d" 11 2 WanderRight] []

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel Init m@Model{..} = noEff m {
    logLines = "Welcome to Scape." : logLines
  }
updateModel (MoveDelta x y) m@Model { player = Mob{x = px, y = py}, .. } = (noEff . step) (exec m (MoveTo (player m) updatePlayer (px+x) (py+y)))
updateModel Wait m@Model{..} = (noEff . step) m {
    logLines = "You wait." : logLines
  }

updatePlayer :: Model -> Mob -> Model
updatePlayer m p = m { player = p }

step :: Model -> Model
step m = foldl ai m mkLenses
  where mkLenses = [(mkGet i, mkSet i) | i <- [0..length (enemies m) - 1]]
        mkGet i = enemies m!!i
        mkSet i m' e' = m' { enemies = take i (enemies m') ++ [e'] ++ drop (i+1) (enemies m')}

ai :: Model -> (Mob, Model -> Mob -> Model) -> Model
ai m@Model{..} (e@Mob{..}, set)
  | state == WanderRight && x == 78 = exec m (ChangeDirection e set)
  | state == WanderRight            = exec m (MoveTo e set (x+1) y)
  | state == WanderLeft && x == 1   = exec m (ChangeDirection e set)
  | state == WanderLeft             = exec m (MoveTo e set (x-1) y)
  | otherwise                       = m

exec :: Model -> Command -> Model
exec m@Model{..} (MoveTo e@Mob{..} set gx gy) = target (findMob m gx gy)
  where -- move into empty spaces
        target Nothing = set m e { x = cx, y = cy }
        -- attack enemies by moving into them
        target (Just e') = exec m (Attack e e')
        -- ignore moves into walls
        cx = clamp 1 78 gx
        cy = clamp 1 22 gy
exec m@Model{..} (Attack Mob{name = sn} Mob{name = dn}) = m {
  logLines = Miso.String.concat ["The ", sn, " attacks the ", dn, "."] : logLines
}
exec m@Model{..} (ChangeDirection e@Mob{..} set) = (set m e { state = reverse state }) {
  logLines = Miso.String.concat ["The ", name, " turns around."] : logLines
}
  where reverse WanderLeft  = WanderRight
        reverse WanderRight = WanderLeft
        reverse s           = s

findMob :: Model -> Int -> Int -> Maybe Mob
findMob m x y = find (hasPos x y) (player m : enemies m)

hasPos :: Int -> Int -> Mob -> Bool
hasPos i j Mob{..} = x == i && y == j

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx