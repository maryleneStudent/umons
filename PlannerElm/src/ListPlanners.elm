module ListPlanners exposing (Model, Msg, init, update, view)

import Appointment exposing (Appointment, appointmentsDecoder)
import Bootstrap.CDN exposing (stylesheet)
import Bootstrap.Grid as Grid
import Dict exposing (Dict, toList)
import Error exposing (buildErrorMessage)
import Html exposing (Html, a, br, div, h3, h5, text)
import Html.Attributes exposing (class, href)
import Http
import Planner exposing (Planner, plannersDecoder)
import RemoteData exposing (WebData)


type alias Model =
    { planners : WebData (List Planner)
    , appointments : WebData (Dict String (List Appointment))
    }


type Msg
    = PlannersReceived (WebData (List Planner))
    | AppointmentsReceived (WebData (Dict String (List Appointment)))


init : Bool -> String -> ( Model, Cmd Msg )
init isAdmin id =
    ( initialModel
    , Cmd.batch
        [ fetchPlanners
        , if String.isEmpty id == False then
            fetchAppoinments isAdmin id

          else
            Cmd.none
        ]
    )


initialModel : Model
initialModel =
    { planners = RemoteData.Loading
    , appointments = RemoteData.Loading
    }


fetchPlanners : Cmd Msg
fetchPlanners =
    Http.get
        { url = "http://localhost:3001/planners/"
        , expect =
            plannersDecoder
                |> Http.expectJson (RemoteData.fromResult >> PlannersReceived)
        }


fetchAppoinments : Bool -> String -> Cmd Msg
fetchAppoinments isAdmin id =
    let
        url =
            if isAdmin == True then
                "http://localhost:3001/doctor/" ++ id ++ "/appointments"

            else
                "http://localhost:3001/patient/" ++ id ++ "/appointments"
    in
    Http.get
        { url = url
        , expect =
            appointmentsDecoder
                |> Http.expectJson (RemoteData.fromResult >> AppointmentsReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlannersReceived response ->
            ( { model | planners = response }, Cmd.none )

        AppointmentsReceived resp ->
            ( { model | appointments = resp }, Cmd.none )



-- VIEWS


view : Model -> Bool -> Html Msg
view model isAdmin =
    Grid.container []
        -- Responsive fixed width container
        [ stylesheet -- Inlined Bootstrap CSS for use with reactor
        , if isAdmin == True then
            text ""

          else
            div [ class "text-center" ] [ viewPlanners model.planners ]
        , br [] []
        , div [] [ viewAppointments isAdmin model.appointments ]
        ]


viewAppointments : Bool -> WebData (Dict String (List Appointment)) -> Html Msg
viewAppointments isAdmin appointments =
    case appointments of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualAppointments ->
            let
                viewer ( key, data ) =
                    div []
                        [ br [] []
                        , h5 [ class "text-left, row, text-primary" ] [ text (String.dropLeft 11 key) ]
                        , viewDatas isAdmin data
                        ]
            in
            div []
                [ br [] []
                , br [] []
                , h5 [ class "text-center" ] [ text "Liste des rendez-vous: " ]
                , actualAppointments
                    |> toList
                    |> List.map viewer
                    |> div []
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewDatas : Bool -> List Appointment -> Html Msg
viewDatas isAdmin datas =
    let
        firstOp =
            viewData isAdmin
    in
    div []
        [ br [] []
        , List.map firstOp datas |> div [ class "row" ]
        ]


viewData : Bool -> Appointment -> Html Msg
viewData isAdmin data =
    if isAdmin == True then
        let
            person =
                data.person
        in
        div [ class "card col-xs-4 col-md-4 col-lg-4" ]
            [ h5 [ class "card-header  bg-info text-white" ] [ text data.hour ]
            , div [ class "card-body" ]
                [ div [ class "card-title" ] [ text (person.firstname ++ " " ++ person.lastname) ]
                ]
            ]

    else
        div [ class "col-xs-4 col-md-4 col-lg-4" ] [ text data.hour ]


viewPlanners : WebData (List Planner) -> Html Msg
viewPlanners planners =
    case planners of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualPlanners ->
            div []
                [ h5 [] [ text "Consultations au cabinet: " ]
                , div [] <| List.map viewPlanner actualPlanners
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewPlanner : Planner -> Html Msg
viewPlanner planner =
    let
        plannerPath =
            "/planners/" ++ Planner.idToString planner.id

        person =
            planner.person
    in
    div [ class "row" ]
        [ div [ class "col-xs-6 col-md-6 col-lg-4" ] [ text person.lastname ]
        , div [ class "col-xs-4 col-md-4 col-lg-4" ]
            [ text person.firstname ]
        , div [ class "col-xs-4 col-md-4 col-lg-4" ]
            [ a [ href plannerPath ] [ text "Prendre rendez-vous" ] ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Consultations impossibles pour le moment"
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]
