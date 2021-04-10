{-# LANGUAGE OverloadedStrings #-}

module Game (initModel, updateModel, viewModel, Action(NoOp)) where

import qualified Data.Map as Map
import Miso
import Miso.String as MS ( MisoString, concat, snoc )

data CellType = Hidden | Floor | Wall | Player | Enemy
  deriving Eq
data Cell = Cell CellType Int Int
  deriving Eq
data Entity = Room | Mob CellType Int Int
  deriving Eq
data Model = Scene {
  scenePlayerX :: Int,
  scenePlayerY :: Int
} deriving Eq

data Action = NoOp

initModel :: Model
initModel = Scene 40 12

updateModel :: Action -> Model -> Effect Action Model
updateModel act = noEff

viewModel :: Model -> View Action
viewModel m = main_ [] [
    link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com"],
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Cutive+Mono&display=swap"],
    link_ [rel_ "stylesheet", href_ "modern-normalize.css"],
    div_ [style_ $ Map.fromList [
      ("background", "blue"),
      ("color", "white"),
      ("height", "100vh"),
      ("display", "grid"),
      ("grid-template-rows", "auto 1fr")
    ]] [
      h1_ [style_ $ Map.singleton "text-align" "center"] [text "Scape!"],
      div_ [style_ $ Map.fromList [("display", "flex"), ("align-items", "center"), ("justify-content", "center")]] [
        viewGame m
      ]
    ]
  ]

viewGame :: Model -> View Action
viewGame (Scene x y) = pre_ [style_ $ Map.fromList [("font-family", "'Cutive Mono', monospace"), ("font-size", "20px")]] [
    text $ render $ draw $ layout [Room, Mob Player x y]
  ]

layout :: [Entity] -> [Cell]
layout []        = []
layout (Room:xs) = [Cell Floor x y | x <- [0..79], y <- [1..23]] ++ 
                   [Cell Wall x 0 | x <- [0..78]] ++
                   [Cell Wall 79 y | y <- [0..22]] ++
                   [Cell Wall x 23 | x <- [1..79]] ++
                   [Cell Wall 0 y | y <- [1..23]] ++ 
                   layout xs
layout (Mob c x y:xs) = Cell c x y : layout xs

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
