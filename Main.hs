{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main(main) where

import Miso
import Update
import View
import Input

#ifndef __GHCJS__
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
import Network.Wai.Application.Static
import Language.Javascript.JSaddle.Warp
#endif

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp frontend = do
  app <- jsaddleWithAppOr 
    defaultConnectionOptions (frontend >> syncPoint) $
    staticApp $ defaultFileServerSettings "wwwroot"
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) app
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

main :: IO ()
main = runApp $ startApp App {..}
  where
    model         = initModel
    initialAction = NoOp
    update        = updateModel'
    view          = viewModel
    events        = defaultEvents
    subs          = [controlSub]
    mountPoint    = Nothing
    logLevel      = Off
