{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View (viewModel) where

import Data.Map as Map
import Miso
import Miso.String
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
      ("grid-template-rows", "1fr auto 1fr")
    ]] [
      div_ [] [
        h1_ [style_ labelStyle] [text "Scape!"],
        viewControls
      ],
      div_ [style_ $ Map.fromList [("display", "grid"), ("grid-template-columns", "1fr auto 1fr"), ("grid-column-gap", "20px"), ("width", "100%")]] [
        div_ [] [],
        viewGame m,
        viewLog m
      ],
      div_ [] []
    ]
  ]

viewControls :: View Action
viewControls = p_ [style_ $ Map.union labelStyle $ Map.fromList [("display", "flex"), ("justify-content", "center")]] [
    section "MOVE (shift=run)" $ alternatives [
      moveGrid ('w', 'e', 'd', 'x', 's', 'z', 'a', 'q'), 
      moveGrid ('k', 'u', 'l', 'n', 'j', 'b', 'h', 'y'), 
      moveGrid ('8', '9', '6', '3', '2', '1', '4', '7')
    ],
    section "WAIT" $ alternatives [text "spacebar", text "5"]
  ]
  where section t c = div_ [] [text t, span_ [style_ $ Map.singleton "font-family" "'Cutive Mono', monospace"] [c]]
        alternatives xs = div_ [style_ $ Map.fromList [
            ("display", "grid"),
            ("grid-auto-flow", "column"),
            ("grid-column-gap", "1px"),
            ("background", "white")
          ]] (fmap alternative xs)
        alternative x = div_ [style_ $ Map.fromList [("background", "blue"), ("padding", "0 1em")]] [x]
        moveGrid :: (Char, Char, Char, Char, Char, Char, Char, Char) -> View Action
        moveGrid (n, ne, e, se, s, sw, w, nw) = span_ [] [
            text $ ms [nw, n, ne], br_ [],
            text $ ms [w, ' ', e], br_ [],
            text $ ms [sw, s, se]
          ]

viewGame :: Model -> View Action
viewGame m = pre_ [style_ $ Map.union monoStyle $ Map.singleton "margin" "0"] [
    text $ renderMap m
  ]

viewLog :: Model -> View Action
viewLog Model{..} = div_ [style_ $ Map.union monoStyle $ Map.fromList [("height", "552px"), ("overflow", "auto")]]
    $ Prelude.concatMap (\l -> [text $ ms l, br_ []]) logLines

monoStyle :: Map MisoString MisoString
monoStyle = Map.fromList [("font-family", "'Cutive Mono', monospace"), ("font-size", "20px")]

labelStyle :: Map MisoString MisoString
labelStyle = Map.fromList [("text-align", "center"), ("user-select", "none")]
