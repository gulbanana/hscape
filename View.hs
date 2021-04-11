{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View (viewModel) where

import qualified Data.Map as Map
import Miso
import Game
import Scene

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
        div_ [style_ $ Map.fromList [("display", "grid"), ("grid-template-columns", "1fr auto 1fr"), ("grid-column-gap", "20px"), ("width", "100%")]] [
          div_ [] [],
          viewGame m,
          viewLog m
        ]
      ]
    ]
  ]

viewGame :: Model -> View Action
viewGame m = pre_ [style_ $ Map.union monoStyle $ Map.singleton "margin" "0"] [
    text $ renderMap m
  ]


viewLog :: Model -> View Action
viewLog Model{..} = div_ [style_ $ Map.union monoStyle $ Map.fromList [("height", "552px"), ("overflow", "auto")]]
    $ concatMap (\l -> [text l, br_ []]) logLines

monoStyle = Map.fromList [("font-family", "'Cutive Mono', monospace"), ("font-size", "20px")]

labelStyle = Map.fromList [("text-align", "center"), ("user-select", "none")]
