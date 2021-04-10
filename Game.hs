{-# LANGUAGE OverloadedStrings #-}

module Game (updateModel, viewModel, Action(NoOp)) where

import Miso
import Miso.String
import qualified Data.Map as Map

data Action = NoOp
type Model = ()

updateModel :: Action -> Model -> Effect Action Model
updateModel act = noEff

viewModel :: Model -> View Action
viewModel _ = main_ [] [
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
        viewGame
      ]
    ]
  ]

viewGame :: View Action
viewGame = pre_ [style_ $ Map.fromList [("font-family", "'Cutive Mono', monospace"), ("font-size", "20px")]] [
  text "........................................\n\
       \........######..........................\n\
       \........#..@.#..........................\n\
       \........#.d..#..........................\n\
       \........######..........................\n\
       \........................................"
  ]