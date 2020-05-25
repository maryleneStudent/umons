module PlannerRoute exposing (PlannerRoute(..), parsePlannerUrl, pushPlannerUrl)

import Browser.Navigation as Nav
import Planner exposing (Planner)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


type PlannerRoute
    = NotFoundPlanner
    | Planners
    | Planner String


parsePlannerUrl : Url -> PlannerRoute
parsePlannerUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFoundPlanner


matchRoute : Parser (PlannerRoute -> a) a
matchRoute =
    oneOf
        [ map Planners top
        , map Planners (s "planners")
        , map Planner (s "planners" </> Planner.idParser)
        ]


pushPlannerUrl : PlannerRoute -> Nav.Key -> Cmd msg
pushPlannerUrl route navKey =
    plannerRouteToString route
        |> Nav.pushUrl navKey


plannerRouteToString : PlannerRoute -> String
plannerRouteToString route =
    case route of
        NotFoundPlanner ->
            "/planners/not-found"

        Planners ->
            "/planners"

        Planner plannerId ->
            "/planners/" ++ plannerId
