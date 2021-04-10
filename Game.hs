{-# LANGUAGE OverloadedStrings #-}

module Game (initModel, updateModel, viewModel, Action(NoOp)) where

import Miso
import Miso.String as MS hiding (replicate, map)
import qualified Data.Map as Map

data Action = NoOp
data Cell = Hidden | Floor | Wall | Player | Enemy
  deriving Eq
type Model = [[Cell]]

initModel :: [[Cell]]
initModel = replicate 24 $ replicate 80 Floor

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
      h1_ [] [text "Scape!"],
      div_ [style_ $ Map.fromList [("display", "flex"), ("align-items", "center"), ("justify-content", "center")]] [
        viewGame m
      ]
    ]
  ]

viewGame :: Model -> View Action
viewGame m = pre_ [style_ $ Map.fromList [("font-family", "'Cutive Mono', monospace"), ("font-size", "20px")]] [
    text $ render m
  ]

render :: Model -> MisoString
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
