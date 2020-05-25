module ViewPlanner exposing (Model, Msg, init, initialModel, update, view)

import Appointment exposing (Appointment, appointmentDecoder, appointmentEncoder, emptyAppointment)
import Bootstrap.Button as Button
import Bootstrap.CDN exposing (stylesheet)
import Bootstrap.Grid as Grid
import Browser.Navigation as Nav
import Dict
import Error exposing (buildErrorMessage)
import Html exposing (Html, br, div, h3, h4, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onFocus)
import Http
import Planner exposing (Data, Planner, entriesToList, plannerDecoder)
import PlannerRoute
import RemoteData exposing (WebData)
import User exposing (User)


type alias Model =
    { navKey : Nav.Key
    , planner : WebData Planner
    , appointment : Appointment
    , saveError : Maybe String
    }


type Msg
    = PlannerReceived (WebData Planner)
    | SelectAppointment String String String String User
    | SaveAppointment
    | AppointmentSaved (Result Http.Error Appointment)


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , planner = RemoteData.Loading
    , appointment = emptyAppointment
    , saveError = Nothing
    }


init : String -> Nav.Key -> ( Model, Cmd Msg )
init plannerId navKey =
    ( initialModel navKey, fetchPlanner plannerId )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlannerReceived response ->
            ( { model | planner = response }, Cmd.none )

        SelectAppointment key value plannerId doctorId user ->
            let
                oldApp =
                    model.appointment

                updateApp =
                    { oldApp | date = key, hour = value, userId = user.id, plannerId = plannerId, doctorId = doctorId, person = user.person }
            in
            ( { model | appointment = updateApp }
            , Cmd.none
            )

        SaveAppointment ->
            ( model, saveAppointment model.appointment )

        AppointmentSaved (Ok appointmentData) ->
            ( { model | appointment = appointmentData, saveError = Nothing }
            , PlannerRoute.pushPlannerUrl PlannerRoute.Planners model.navKey
            )

        AppointmentSaved (Err error) ->
            ( { model | saveError = Just (buildErrorMessage error) }
            , Cmd.none
            )


saveAppointment : Appointment -> Cmd Msg
saveAppointment appointment =
    let
        appointmentUrl =
            "http://localhost:3001/appointment/create"
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = appointmentUrl
        , body = Http.jsonBody (appointmentEncoder appointment)
        , expect = Http.expectJson AppointmentSaved appointmentDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchPlanner : String -> Cmd Msg
fetchPlanner plannerId =
    Http.get
        { url = "http://localhost:3001/planners/" ++ plannerId
        , expect =
            plannerDecoder
                |> Http.expectJson (RemoteData.fromResult >> PlannerReceived)
        }



-- VIEWS


view : Model -> User -> Html Msg
view model user =
    Grid.container []
        -- Responsive fixed width container
        [ stylesheet -- Inlined Bootstrap CSS for use with reactor
        , viewPlanner model.planner user
        , viewSaveError model.saveError
        ]


viewPlanner : WebData Planner -> User -> Html Msg
viewPlanner planner user =
    case planner of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualPlanner ->
            let
                person =
                    actualPlanner.person
            in
            div
                []
                [ h3 [] [ text ("Prendre un rendez-vous chez le praticien " ++ person.lastname ++ " " ++ person.firstname) ]
                , viewThisPlanner actualPlanner user
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewThisPlanner : Planner -> User -> Html Msg
viewThisPlanner planner user =
    div []
        [ br [] []
        , div [] [ displayPlanner planner user ]
        ]


viewButtonValue : String -> String -> User -> Data -> Html Msg
viewButtonValue plannerId doctorId user appointment =
    div [ class "col-xs-6 col-md-6 col-lg-4" ]
        [ Button.button
            [ Button.light
            , Button.block
            , Button.attrs [ onFocus (SelectAppointment appointment.key appointment.value plannerId doctorId user), onClick SaveAppointment ]
            ]
            [ text appointment.value ]
        ]


displayPlanner : Planner -> User -> Html Msg
displayPlanner planner user =
    let
        viewer ( key, data ) =
            div []
                [ h4 [ class "row, text-primary" ] [ text (String.dropLeft 11 key) ]
                , displayData (Planner.idToString planner.id) planner.doctorId user key (Planner.entriesToList data)
                ]
    in
    div []
        (Planner.getRemainingDays planner
            |> Dict.toList
            |> List.map viewer
        )


displayData : String -> String -> User -> String -> List String -> Html Msg
displayData plannerId doctorId user key data =
    let
        firstPart =
            viewButtonValue plannerId doctorId user
    in
    List.map firstPart (Planner.toDataListWithKey key (Planner.toDataList data))
        |> div [ class "row" ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Les horaires de consultation ne sont pas disponibles en ce moment. Veuillez réessayer plus tard"
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Il y a eu une erreur lors de la prise du rendez-vous. Veuillez réessayer plus tard." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
