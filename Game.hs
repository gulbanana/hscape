{-# LANGUAGE OverloadedStrings #-}

module Game (initModel, updateModel, viewModel, Action(NoOp)) where

import qualified Data.Map as Map
import Miso
import Miso.String as MS ( MisoString, concat, snoc )

data Cell = Hidden | Floor | Wall | Player | Enemy
  deriving Eq
data Entity = Entity Cell Int Int
  deriving Eq
type Model = [Entity]

data Action = NoOp

initModel :: [Entity]
initModel = [Entity Floor x y | x <- [0..79], y <- [1..23]] ++ 
            [Entity Wall x 0 | x <- [0..78]] ++
            [Entity Wall 79 y | y <- [0..22]] ++
            [Entity Wall x 23 | x <- [1..79]] ++
            [Entity Wall 0 y | y <- [1..23]] ++
            [Entity Player 40 12]

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
viewGame m = pre_ [style_ $ Map.fromList [("font-family", "'Cutive Mono', monospace"), ("font-size", "20px")]] [
    text $ render $ layout m
  ]

-- TODO: apply the obvious optimisation, creating hidden cells only when no entity is present
layout :: Model -> [[Cell]]
layout = foldl place base
  where base = replicate 24 (replicate 80 Hidden)
        place cs (Entity c x y) = replace2 y x c cs
        replace1 i x xs = 
          take i xs ++ (x : drop (i+1) xs)
        replace2 j i x xs =
          let row_to_replace_in = xs !! j
              modified_row = replace1 i x row_to_replace_in
          in replace1 j modified_row xs

render :: [[Cell]] -> MisoString
render = MS.concat . map (mkLine . renderLine)
  where mkLine = flip snoc '\n'

renderLine :: [Cell] -> MisoString
renderLine = MS.concat . map renderCell

renderCell :: Cell -> MisoString
renderCell Hidden = " "
renderCell Floor  = "."
renderCell Wall   = "#"
renderCell Player = "@"
renderCell Enemy  = "d"
