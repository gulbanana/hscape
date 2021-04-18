module Update (Action(..), initModel, updateModel') where

import Miso
import Game

updateModel' :: Action -> Model -> Effect Action Model
updateModel' a = noEff . updateModel a