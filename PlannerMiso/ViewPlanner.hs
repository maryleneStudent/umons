{-# LANGUAGE OverloadedStrings #-}
module ViewPlanner where

import           Miso
import           Miso.String (ms)
import           Miso.String
import           Action
import           Model
import           Routing
import           Data.Map

viewPlanner :: Planner -> User -> View Action
viewPlanner p u = div_ []
    [ nav $ person p
    , form p u
    ]

nav :: Person -> View Action
nav p = div_
    [ class_ "m3" ]
    [ h5_ [] [ text . ms $ "Prendre un rendez-vous chez le praticien " ++ lastname p ++ " " ++ firstname p ]
    ]

form :: Planner -> User -> View Action
form p u = div_ [] [viewMapWithUser $ remainingDays p]
  where viewMapWithUser = viewMap p u

viewMap :: Planner -> User -> Map String Entries -> View Action
viewMap p u remainingDays =
  div_ [] $ Prelude.map firstCall (keys remainingDays)
  where firstCall = viewKey remainingDays p u

viewKey :: Map String Entries -> Planner -> User -> String -> View Action
viewKey map p u k =  div_ [] [br_[], h4_ [class_ "row text-primary"][ text $ pack $ Prelude.drop 11 k]
                   , div_ [] [viewValuesWithKey $ Data.Map.lookup k map]]
  where viewValuesWithKey = viewValues k p u

viewValues :: String -> Planner -> User -> Maybe Entries -> View Action
viewValues key p u entries = case entries of Just e -> div_ [] [viewEntries key e p u]
                                             Nothing -> div_ [][]

getEntries :: Entries -> [String]
getEntries (Entries e) = e

viewEntries :: String -> Entries -> Planner -> User -> View Action
viewEntries key entries planner user = div_ [class_ "row"] $ Prelude.map viewValueWithKey entryList
  where entryList = getEntries entries
        viewValueWithKey = viewValue key planner user

viewValue :: String -> Planner -> User -> String -> View Action
viewValue k p user v = div_ [class_ "col-xs-6 col-md-6 col-lg-3"][button_ [ class_ "btn btn-block btn-light", onClick $ SaveAppointment k v plannerid docId user] [ text $ pack v ]]
  where plannerid = Model.id p
        docId = doctorId p
