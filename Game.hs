{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game (initModel, updateModel, viewModel, Action(..)) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Miso
import Miso.String as MS ( MisoString, concat, snoc )
import Data.Function
import Data.List

data CellType = Hidden | Floor | Wall | Player | Enemy
  deriving Eq
data Cell = Cell CellType Int Int
  deriving Eq
data Entity = Room | Tile CellType Int Int
  deriving Eq
data Mob = Mob Int Int
  deriving Eq
data Model = Scene {
  scenePlayerX :: Int,
  scenePlayerY :: Int,
  enemies :: [Mob],
  logLines :: [MisoString]
} deriving Eq

initModel :: Model
initModel = Scene 40 12 [Mob 1 21, Mob 3 6] []

data Action = NoOp
            | Init
            | MoveDelta Int Int
            | Wait
            | Log MisoString

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel Init m@Scene{..} = noEff m {
    logLines = "Welcome to Scape." : logLines
  }
updateModel (MoveDelta x y) m@Scene{..} = (noEff . step) (exec (MoveTo (scenePlayerX+x) (scenePlayerY+y)) m)
updateModel Wait m@Scene{..} = (noEff . step) m {
    logLines = "You wait." : logLines
  }

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

step :: Model -> Model
step m@Scene{..} = m {
    enemies = map (ai m) enemies
  }

ai :: Model -> Mob -> Mob
ai m@Scene{..} (Mob x y) 
          | scenePlayerX == cx && scenePlayerY == cy = Mob x y
          | isJust (findMob m cx cy)                 = Mob x y
          | otherwise                                = Mob cx cy
  where gx = x+1
        gy = y
        cx = clamp 1 78 gx
        cy = clamp 1 22 gy

data Command = MoveTo Int Int
             | Attack Mob

exec :: Command -> Model -> Model
exec (MoveTo x y) m = target (findMob m x y)
  where target Nothing = m {
          scenePlayerX = cx,
          scenePlayerY = cy
        }
        -- attack enemies by moving into them
        target (Just e) = exec (Attack e) m
        -- ignore moves into walls
        cx = clamp 1 78 x
        cy = clamp 1 22 y
exec (Attack e) m@Scene{..} = m {
  logLines = "You attack." : logLines
}

findMob :: Model -> Int -> Int -> Maybe Mob
findMob m x y = find (mobHasPos x y) (enemies m)

mobHasPos :: Int -> Int -> Mob -> Bool
mobHasPos i j (Mob x y) = x == i && y == j

viewModel :: Model -> View Action
viewModel m = main_ [] [
#ifndef __GHCJS__
    link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com"],
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Cutive+Mono&display=swap"],
    link_ [rel_ "stylesheet", href_ "modern-normalize.css"],
#endif
    div_ [style_ $ Map.fromList [
      ("background", "blue"),
      ("color", "white"),
      ("height", "100vh"),
      ("display", "grid"),
      ("grid-template-rows", "auto auto 1fr")
    ]] [
      h1_ [style_ labelStyle] [text "Scape!"],
      p_ [style_ labelStyle] [text "MoveDelta: wasd | 8426 | ↑←↓→", br_ [], text "Wait: spacebar | 5"],
      div_ [style_ $ Map.fromList [("display", "flex"), ("align-items", "center"), ("justify-content", "center")]] [
        div_ [style_ $ Map.fromList [("display", "grid"), ("grid-template-columns", "1fr auto 1fr"), ("grid-column-gap", "20px")]] [
          div_ [] [],
          viewGame m,
          viewLog m
        ]
      ]
    ]
  ]

viewGame :: Model -> View Action
viewGame Scene{..} = pre_ [style_ $ Map.union monoStyle $ Map.singleton "margin" "0"] [
    text $ render $ draw $ layout ([Room, Tile Player scenePlayerX scenePlayerY] ++ map viewMob enemies)
  ]

viewMob :: Mob -> Entity
viewMob (Mob x y) = Tile Enemy x y

layout :: [Entity] -> [Cell]
layout []        = []
layout (Room:xs) = [Cell Floor x y | x <- [0..79], y <- [1..23]] ++
                   [Cell Wall x 0 | x <- [0..78]] ++
                   [Cell Wall 79 y | y <- [0..22]] ++
                   [Cell Wall x 23 | x <- [1..79]] ++
                   [Cell Wall 0 y | y <- [1..23]] ++
                   layout xs
layout (Tile c x y:xs) = Cell c x y : layout xs

-- TODO: apply the obvious optimisation, creating hidden cells only when no entity is present
draw :: [Cell] -> [[CellType]]
draw = foldl place base
  where base = replicate 24 (replicate 80 Hidden)
        place cs (Cell c x y) = replace2 y x c cs
        replace1 i x xs =
          take i xs ++ (x : drop (i+1) xs)
        replace2 j i x xs =
          let row_to_replace_in = xs !! j
              modified_row = replace1 i x row_to_replace_in
          in replace1 j modified_row xs

render :: [[CellType]] -> MisoString
render = MS.concat . map (mkLine . renderLine)
  where mkLine = flip snoc '\n'

renderLine :: [CellType] -> MisoString
renderLine = MS.concat . map renderCell

renderCell :: CellType -> MisoString
renderCell Hidden = " "
renderCell Floor  = "."
renderCell Wall   = "#"
renderCell Player = "@"
renderCell Enemy  = "d"

viewLog :: Model -> View Action
viewLog Scene{..} = div_ [style_ $ Map.union monoStyle $ Map.fromList [("height", "552px"), ("overflow", "auto")]]
    $ concatMap (\l -> [text l, br_ []]) logLines

monoStyle = Map.fromList [("font-family", "'Cutive Mono', monospace"), ("font-size", "20px")]

labelStyle = Map.fromList [("text-align", "center"), ("user-select", "none")]
