{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Routing where

import           Data.Proxy
import           Miso
import           Servant.API
import           Servant.Links

import           Action
import           Model

type Route =
         TopRoute
    :<|> ListRoute
    :<|> EditRoute

type TopRoute =  View Action

type ListRoute = "planners" :> View Action

type EditRoute = "planners" :> Capture "id" PlannerId :> View Action

topLink :: URI
topLink = linkURI $ safeLink (Proxy :: Proxy Route) (Proxy :: Proxy TopRoute)

listLink :: URI
listLink =  linkURI $ safeLink (Proxy :: Proxy Route) (Proxy :: Proxy ListRoute)

editLink :: PlannerId -> URI
editLink i =  linkURI $ safeLink (Proxy :: Proxy Route) (Proxy :: Proxy EditRoute) i

