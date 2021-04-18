module Input (controlSub) where

import Data.List
import Data.Maybe
import Data.Set
import Miso
import Game

data Control = ControlN | ControlS | ControlW | ControlE 
             | ControlNW | ControlNE | ControlSW | ControlSE
             | ControlPause
             | ModifierShift
  deriving (Eq, Ord)

controlSub :: Sub Action
controlSub = keyboardSub (pick . sort . mapMaybe controls . toList)

controls :: Int -> Maybe Control
controls 16  = Just ModifierShift
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
-- numpad (and, with shift on windows, arrows!)
controls 97  = Just ControlSW
controls 35  = Just ControlSW
controls 98  = Just ControlS
controls 40  = Just ControlS
controls 99  = Just ControlSE
controls 34  = Just ControlSE
controls 100 = Just ControlW
controls 37  = Just ControlW
controls 102 = Just ControlE
controls 39  = Just ControlE
controls 103 = Just ControlNW
controls 36  = Just ControlNW
controls 104 = Just ControlN
controls 38  = Just ControlN
controls 105 = Just ControlNE
controls 33  = Just ControlNE
-- 5, space
controls 101 = Just ControlPause
controls 32  = Just ControlPause
controls _  = Nothing 

pick :: [Control] -> Action
pick [ControlN]                 = Walk 0 (-1)
pick [ControlN, ModifierShift]  = Run 0 (-1)
pick [ControlNE]                = Walk 1 (-1)
pick [ControlNE, ModifierShift] = Run 1 (-1)
pick [ControlE]                 = Walk 1 0
pick [ControlE, ModifierShift]  = Run 1 0
pick [ControlSE]                = Walk 1 1
pick [ControlSE, ModifierShift] = Run 1 1
pick [ControlS]                 = Walk 0 1
pick [ControlS, ModifierShift]  = Run 0 1
pick [ControlSW]                = Walk (-1) 1
pick [ControlSW, ModifierShift] = Run (-1) 1
pick [ControlW]                 = Walk (-1) 0
pick [ControlW, ModifierShift]  = Run (-1) 0
pick [ControlNW]                = Walk (-1) (-1)
pick [ControlNW, ModifierShift] = Run (-1) (-1)
pick [ControlPause]             = Wait
pick _                          = NoOp 
