{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module View where

import           Action
import           Model
import           ViewPlanner
import           ListPlanners
import           Home
import           Html
import           Routing

import           Data.List
import           Data.Maybe
import           Data.Proxy
import qualified Lucid                         as L
import qualified Data.Map                      as M
import           Miso
import           Miso.String (ms, pack)
import           Servant.API

viewModel :: Model -> View Action
viewModel m@Model{..} = div_ [class_ "container"] [ nav_ [ class_ "navbar navbar-light justify-content-between", style_ $ M.fromList [(pack "background-color", pack "#e3f2fd")] ]
            [ a_ [ class_ "navbar-brand" ] [ text $ pack "Mes rendez-vous" ]
            , text $ pack firstNameVal
            , if auth == LoggedOut then
                text $ pack ""

              else
                button_
                    [ class_ "btn btn-outline-danger my-2 my-sm-0", onClick LogOut ]
                    [ text $ pack "Déconnexion" ]
            ], page m ]
             where usr = getUserFromStatus auth
                   firstNameVal = case usr of Nothing -> ""
                                              Just u -> getUserFirstname u ++ " " ++ getUserLastname u

getUserFirstname :: User -> String
getUserFirstname user = firstname (uPerson user)

getUserLastname :: User -> String
getUserLastname user = lastname (uPerson user)

page :: Model -> View Action
page m = view
          where
           view = 
            either (const notFoundPage) Prelude.id
            $ runRoute (Proxy :: Proxy Route) handlers currentURI m
              where
                handlers = topPage :<|> listPage :<|> editPage

topPage :: Model -> View Action
topPage = homePage

homePage :: Model -> View Action
homePage m = viewLoginOrRegister m

listPage :: Model -> View Action
listPage m@Model{..} = div_ [][ viewPlanners planners isAdmin, br_[], viewAppointments appointments isAdmin ]

editPage :: PlannerId -> Model -> View Action
editPage query m@Model{..} =
  case planners of
    Right ps -> case find (\p -> Model.id p == query) ps of
        Just p -> case user of Just u -> viewPlanner p u
                               Nothing -> notAllowedPage
          where user = getUserFromStatus auth
        Nothing -> notFoundPage
    Left err ->  errorPage err

getUserFromStatus :: AuthenticationStatus -> Maybe User
getUserFromStatus status =  case status of
  LoggedIn _ u -> Just u
  LoggedOut -> Nothing
  Unknown -> Nothing

notFoundPage :: View action
notFoundPage = errorPage "Page non trouvée"

notAllowedPage :: View action
notAllowedPage = errorPage "Vous n'étes pas autorisé à consulter cette page"

errorPage :: String -> View action
errorPage err = div_ []
    [ div_ [ class_ "clearfix mb2 white bg-black p1" ] []
    , div_ [ class_ "p2" ] [ text . ms $ err ]
    ]
