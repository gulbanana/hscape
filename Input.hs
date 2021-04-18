module Input (controlSub) where

import Data.Maybe
import Data.Set
import Miso
import Game

data Control = ControlN | ControlS | ControlW | ControlE 
             | ControlNW | ControlNE | ControlSW | ControlSE
             | ControlPause
  deriving (Eq, Ord)

controlSub :: Sub Action
controlSub = keyboardSub (pickIfOne . mapMaybe controls . toList)

controls :: Int -> Maybe Control
-- wasd
controls 87  = Just ControlN
controls 65  = Just ControlW
controls 83  = Just ControlS
controls 68  = Just ControlE
-- qezc
controls 81  = Just ControlNW
controls 69  = Just ControlNE
controls 90  = Just ControlSW
controls 67  = Just ControlSE
-- hjkl
controls 72  = Just ControlW
controls 74  = Just ControlS
controls 75  = Just ControlN
controls 76  = Just ControlE
-- yubn
controls 89  = Just ControlNW
controls 85  = Just ControlNE
controls 66  = Just ControlSW
controls 78  = Just ControlSE
-- numpad
controls 97  = Just ControlSW
controls 98  = Just ControlS
controls 99  = Just ControlSE
controls 100 = Just ControlW
controls 102 = Just ControlE
controls 103 = Just ControlNW
controls 104 = Just ControlN
controls 105 = Just ControlNE
-- 5, space
controls 101 = Just ControlPause
controls 32  = Just ControlPause
controls _  = Nothing 

pickIfOne :: [Control] -> Action
pickIfOne []  = NoOp
pickIfOne [x] = pick x
pickIfOne _   = NoOp

pick :: Control -> Action
pick ControlN  = MoveDelta 0 (-1)
pick ControlNE = MoveDelta 1 (-1)
pick ControlE  = MoveDelta 1 0
pick ControlSE = MoveDelta 1 1
pick ControlS  = MoveDelta 0 1
pick ControlSW = MoveDelta (-1) 1
pick ControlW  = MoveDelta (-1) 0
pick ControlNW = MoveDelta (-1) (-1)
pick ControlPause = Wait
