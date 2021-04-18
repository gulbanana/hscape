{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Input (controlSub) where

import Data.List
import Data.Maybe
import Data.Set as S
import Miso
import Game
import Control.Monad.IO.Class
import Data.IORef
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal

data Control = ControlN | ControlS | ControlW | ControlE
             | ControlNW | ControlNE | ControlSW | ControlSE
             | ControlPause
             | ModifierShift
  deriving (Eq, Ord)

controlSub :: Sub Action
controlSub = keyCodeSub (pick . sort . mapMaybe controls . toList)

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
-- numpad
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

-- |like keyboardSub, but with special handling for numpad
keyCodeSub :: (Set Int -> action) -> Sub action
keyCodeSub f sink = do
  keySetRef <- liftIO (newIORef mempty)
  windowAddEventListener "keyup" $ keyUpCallback keySetRef
  windowAddEventListener "keydown" $ keyDownCallback keySetRef
  windowAddEventListener "blur" $ blurCallback keySetRef
    where
      keyDownCallback keySetRef = \keyDownEvent -> do
          (key, shift) <- deriveKeyCode keyDownEvent
          newKeys <- liftIO $ atomicModifyIORef' keySetRef $ \keys ->
             let !new = S.insert key keys
             in (new, new)
          let keysWithShift = if shift then S.insert 16 newKeys else newKeys
          liftIO (sink (f keysWithShift))

      keyUpCallback keySetRef = \keyUpEvent -> do
          (key, shift) <- deriveKeyCode keyUpEvent
          newKeys <- liftIO $ atomicModifyIORef' keySetRef $ \keys ->
             let !new = S.delete key keys
             in (new, new)
          let keysWithShift = if shift then S.insert 16 newKeys else newKeys
          liftIO (sink (f keysWithShift))

      blurCallback keySetRef = \_ -> do
          newKeys <- liftIO $ atomicModifyIORef' keySetRef $ \_ ->
            let !new = S.empty
            in (new, new)
          liftIO (sink (f newKeys))

      deriveKeyCode = \event -> do
        Just (kc::Int) <- fromJSVal =<< getProp "keyCode" (Object event)
        Just (key::String) <- fromJSVal =<< getProp "key" (Object event)
        Just (code::String) <- fromJSVal =<< getProp "code" (Object event)
        return (case (code, key) of
          ("Numpad1", "End")        -> (97, True)
          ("Numpad2", "ArrowDown")  -> (98, True)
          ("Numpad3", "PageDown")   -> (99, True)
          ("Numpad4", "ArrowLeft")  -> (100, True)
          ("Numpad5", "Clear")      -> (101, True)
          ("Numpad6", "ArrowRight") -> (102, True)
          ("Numpad7", "Home")       -> (103, True)
          ("Numpad8", "ArrowUp")    -> (104, True)
          ("Numpad9", "PageUp")     -> (105, True)          
          _                         -> (kc, False))
      