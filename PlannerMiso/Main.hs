{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Miso

import           Action
import           Model
import           Update
import           Miso.String
import           ListPlanners
import           View
import           Network.URI
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080

-- | Main entry point
main :: IO ()
main = 
    runApp $ do
      uri <- getCurrentURI
      startApp App { model = initialModel uri, mountPoint = Nothing, .. }
    where
      initialAction = initAction
      update = updateModel
      events = defaultEvents
      subs   = [uriSub HandleURI ]
      view   = viewModel

