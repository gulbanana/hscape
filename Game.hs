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
viewModel _ = div_ [] [
    div_ [style_ $ Map.fromList [
      ("background", "blue"),
      ("color", "white"),
      ("height", "100vh"),
      ("display", "grid"),
      ("grid-template-rows", "auto 1fr")
    ]] [
      h1_ [] [text "Scape!"],
      div_ [style_ $ Map.fromList [("display", "flex"), ("align-items", "center"), ("justify-content", "center")]] [
        p_ [] [text "Hello, world."]
      ]
    ]
  ]
