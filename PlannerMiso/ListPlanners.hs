{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ListPlanners where

import           Miso
import           Miso.String
import           Miso.String (ms)

import           Action
import           Model
import           Routing
import           Data.Map

viewPlanners :: Either String [Planner] -> Bool -> View Action
viewPlanners ePs isAdmin = div_ [hidden_ True | isAdmin]
    [ list ePs
    ]

viewAppointments :: Either String (Map String [Appointment]) -> Bool -> View Action
viewAppointments apps isAdmin = div_ []
    [ navApps
    , viewMap apps isAdmin]

navApps :: View Action
navApps = div_ [][ br_ [], br_ [], h5_ [ class_ "text-center" ] [ text "Liste des rendez-vous: " ] ]

viewMap :: Either String (Map String [Appointment]) -> Bool -> View Action
viewMap (Left msg) _ = div_ [][text $ ms msg]
viewMap (Right app ) isAdmin = div_ [][ displayMap app isAdmin]
 
list :: Either String [Planner] -> View Action
list (Left msg) = div_ [ class_ "p2" ] [ text $ ms msg ]
list (Right ps) = div_
    []
    [ br_ [], h5_ [] [ text "Consultations au cabinet: " ]
    , div_ [] $ Prelude.map plannerRow ps
    ]

plannerRow :: Planner -> View Action
plannerRow p = view where
    view = div_ [ class_ "row" ]
        [ div_ [ class_ "col-xs-6 col-md-6 col-lg-4" ] [ text $ lastName $ Model.person p ]
        , div_ [ class_ "col-xs-4 col-md-4 col-lg-4" ] [ text $ firstName $ Model.person p ]
        , div_ [ class_ "col-xs-4 col-md-4 col-lg-4" ] [ button_ [class_ "btn btn-link", onClick $ ChangeURI $ editLink (Model.id p) ] [ text "Prendre rendez-vous" ] ]
        ]

displayMap :: Map String [Appointment] -> Bool -> View Action
displayMap apps isAdmin = 
  div_ [] $ Prelude.map firstCall (keys apps)
  where firstCall = viewKey apps isAdmin

viewKey :: Map String [Appointment] -> Bool -> String -> View Action
viewKey map isAdmin k =  div_ [] [br_ [], h5_ [class_ "text-left, row, text-primary"][ text $ pack $ Prelude.drop 11 k]
                       , div_ [] [viewApts isAdmin $ Data.Map.lookup k map]]

viewApts :: Bool -> Maybe [Appointment] -> View Action
viewApts isAdmin apps = case apps of Just a -> div_ [][br_[] , div_ [class_ "row"] $ Prelude.map viewAppIsAdmin a]
                                        where viewAppIsAdmin = viewApp isAdmin
                                     Nothing -> div_ [][]

viewApp :: Bool -> Appointment -> View Action
viewApp isAdmin app@Appointment{..} = if isAdmin == True then         
        div_ [ class_ "card col-xs-4 col-md-4 col-lg-4" ]
            [ h5_ [ class_ "card-header  bg-info text-white" ] [ text $ pack hour ]
            , div_ [ class_ "card-body" ]
                [ div_ [ class_ "card-title" ] [ text . ms $ firstname aPerson ++ " " ++ lastname aPerson ]
                ]
            ] else div_ [class_ "col-xs-4 col-md-4 col-lg-4"]  [ text $ pack hour ]

appRow :: String -> View Action
appRow k = div_ [ class_ "row"] [text $ pack k]
     
firstName :: Person -> MisoString
firstName (Person firstname _) = ms firstname

lastName :: Person -> MisoString
lastName (Person _ lastname) = ms lastname

