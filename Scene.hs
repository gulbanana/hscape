{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scene (renderMap) where

import Data.Function
import Miso
import Miso.String as MS ( MisoString, concat, snoc )
import Game

data CellType = Hidden | Floor | Wall | Tile MisoString
  deriving Eq

data Cell = Cell CellType Int Int
  deriving Eq

data Entity = Room | Mobile Mob
  deriving Eq

renderMap :: Model -> MisoString
renderMap = render . draw . layoutScene . createScene

createScene :: Model -> [Entity]
createScene Model{..} = Room : Mobile player : map Mobile enemies

layoutScene :: [Entity] -> [Cell]
layoutScene = concatMap layoutNode

layoutNode :: Entity -> [Cell]
layoutNode Room =             [Cell Floor x y | x <- [0..79], y <- [1..23]] ++
                              [Cell Wall x 0 | x <- [0..78]] ++
                              [Cell Wall 79 y | y <- [0..22]] ++
                              [Cell Wall x 23 | x <- [1..79]] ++
                              [Cell Wall 0 y | y <- [1..23]]
layoutNode (Mobile Mob{..}) = [Cell (Tile sym) x y]

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
renderCell Hidden   = " "
renderCell Floor    = "."
renderCell Wall     = "#"
renderCell (Tile t) = t