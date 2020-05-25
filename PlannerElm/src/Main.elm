port module Main exposing (Model, Msg(..), Planner(..), currentView, init, initCurrentPlanner, main, notFoundView, tokenDecoder, update, view)

import Bootstrap.CDN exposing (stylesheet)
import Bootstrap.Grid as Grid
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Credential exposing (Credential, credentialDecoder, emptyCredential, emptyPerson, personDecoder, userEncoder)
import Debug exposing (toString)
import Html exposing (Html, a, br, button, div, h2, h3, input, nav, p, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, map6, string)
import Json.Decode.Pipeline exposing (required)
import ListPlanners as ListPlanners
import PlannerRoute exposing (PlannerRoute)
import Route exposing (Route)
import Url exposing (Url)
import User exposing (User)
import ViewPlanner


type alias Model =
    { route : Route
    , plannerRoute : PlannerRoute
    , planner : Planner
    , auth : LoggedInStatus
    , fetchUserErr : String
    , credential : Credential
    , isAdmin : Bool
    , register : Bool
    , navKey : Nav.Key
    }


type Planner
    = NotFoundPlanner
    | ListPlanner ListPlanners.Model
    | ViewPlannerById ViewPlanner.Model


type LoggedInStatus
    = Unknown
    | LoggedOut
    | LoggedIn String User


type alias Token =
    { timestamp : Maybe String
    , status : Maybe Int
    , error : Maybe String
    , message : Maybe String
    , id_token : Maybe String
    , is_admin : Maybe Bool
    }


type alias Flags =
    { storedToken : Maybe String
    , storedUser : Maybe User
    , storedIsAdmin : Maybe Bool
    }


type Msg
    = ListPlannerMsg ListPlanners.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | ViewPlannerMsg ViewPlanner.Msg
    | SetUsername String
    | SetPassword String
    | SetFirstName String
    | SetLastName String
    | ClickRegisterUser
    | GetTokenCompleted (Result Http.Error Token)
    | RegisterCompleted (Result Http.Error Credential)
    | ClickLogIn
    | LogOut
    | FetchedUser String User
    | ErrorFetchingUser String
    | DisplayRegistrationView


port setStorage : String -> Cmd msg


port removeStorage : String -> Cmd msg


port setUserInLocalStorage : User -> Cmd msg


port removeUserFromLocalStorage : User -> Cmd msg


port setIsAdminInLocalStorage : Bool -> Cmd msg


port removeIsAdminFromLocalStorage : Bool -> Cmd msg


emptyUser : User
emptyUser =
    { id = "", person = emptyPerson }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        setCredential =
            case flags.storedToken of
                Just tok ->
                    case flags.storedUser of
                        Just usr ->
                            { id = "", username = "", password = "", token = tok, errorMsg = "", person = usr.person }

                        Nothing ->
                            { id = "", username = "", password = "", token = tok, errorMsg = "", person = emptyPerson }

                Nothing ->
                    emptyCredential

        setIsLoggedIn =
            case flags.storedToken of
                Just _ ->
                    Unknown

                Nothing ->
                    LoggedOut

        isAdmin =
            case flags.storedIsAdmin of
                Just ad ->
                    ad

                Nothing ->
                    False

        commands =
            case flags.storedToken of
                Just tok ->
                    Cmd.batch [ setStorage tok, fetchUserInformation tok ]

                Nothing ->
                    Cmd.none

        model =
            { route = Route.parseUrl url
            , plannerRoute = PlannerRoute.parsePlannerUrl url
            , planner = NotFoundPlanner
            , auth = setIsLoggedIn
            , isAdmin = isAdmin
            , fetchUserErr = ""
            , credential = setCredential
            , register = False
            , navKey = navKey
            }
    in
    initCurrentPlanner ( model, commands )


initCurrentPlanner : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPlanner ( model, existingCmds ) =
    let
        ( currentPlanner, mappedPlannerCmds ) =
            case model.plannerRoute of
                PlannerRoute.NotFoundPlanner ->
                    ( NotFoundPlanner, Cmd.none )

                PlannerRoute.Planner plannerId ->
                    let
                        ( viewModel, viewPlannerCmd ) =
                            ViewPlanner.init plannerId model.navKey
                    in
                    ( ViewPlannerById viewModel, Cmd.map ViewPlannerMsg viewPlannerCmd )

                PlannerRoute.Planners ->
                    case model.auth of
                        LoggedIn _ user ->
                            let
                                ( listModel, listPlannerCmd ) =
                                    ListPlanners.init model.isAdmin user.id
                            in
                            ( ListPlanner listModel, Cmd.map ListPlannerMsg listPlannerCmd )

                        _ ->
                            ( NotFoundPlanner, Cmd.none )
    in
    ( { model | planner = currentPlanner }
    , Cmd.batch [ existingCmds, mappedPlannerCmds ]
    )


view : Model -> Document Msg
view model =
    { title = "Scheduler App"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    Grid.container []
        -- Responsive fixed width container
        [ stylesheet -- Inlined Bootstrap CSS for use with reactor
        , nav [ class "navbar navbar-light justify-content-between", style "background-color" "#e3f2fd" ]
            [ a [ class "navbar-brand" ] [ text "Mes rendez-vous" ]
            , let
                oldModel =
                    model.auth

                firstNameVal =
                    case oldModel of
                        LoggedIn _ u ->
                            let
                                pers =
                                    u.person
                            in
                            pers.firstname ++ " " ++ pers.lastname

                        _ ->
                            ""
              in
              text firstNameVal
            , if model.auth == LoggedOut then
                text ""

              else
                button
                    [ class "btn btn-outline-danger my-2 my-sm-0", onClick LogOut ]
                    [ text "Déconnexion" ]
            ]
        , case model.auth of
            LoggedIn _ user ->
                case model.planner of
                    NotFoundPlanner ->
                        notFoundView

                    ListPlanner plannerModel ->
                        ListPlanners.view plannerModel model.isAdmin
                            |> Html.map ListPlannerMsg

                    ViewPlannerById plannerModel ->
                        ViewPlanner.view plannerModel user
                            |> Html.map ViewPlannerMsg

            _ ->
                defaultView model
        ]


defaultView : Model -> Html Msg
defaultView model =
    let
        credential =
            model.credential

        authBoxView =
            let
                -- If there is an error on authentication, show the error alert
                showError : Html.Attribute msg
                showError =
                    if String.isEmpty credential.errorMsg then
                        style "display" "none"

                    else
                        style "display" "inline"
            in
            div
                []
                [ h2 [ class "text-center" ] [ stylesheet, text "Identification ou enregistrement" ]
                , p [ class "help-block" ] [ stylesheet, text "Si vous avez déjà un compte, veuillez vous identifier. Sinon, créez un nouveau compte." ]
                , div [ showError ]
                    [ div [ class "alert alert-danger" ] [ stylesheet, text credential.errorMsg ]
                    ]
                , div []
                    [ if model.register == False then
                        Html.form []
                            [ showBaseForm credential
                            , div []
                                [ stylesheet
                                , button [ class "btn btn-primary", type_ "button", onClick ClickLogIn ]
                                    [ text "S'identifier" ]
                                , button [ class "btn btn-link", type_ "button", onClick DisplayRegistrationView ]
                                    [ text "S'enregistrer" ]
                                ]
                            ]

                      else
                        let
                            newPerson =
                                credential.person
                        in
                        Html.form []
                            [ showBaseForm credential
                            , div []
                                [ text "Prénom"
                                , br [] []
                                , viewInput "text" "" newPerson.firstname SetFirstName
                                ]
                            , br [] []
                            , div []
                                [ text "Nom"
                                , br [] []
                                , viewInput "text" "" newPerson.lastname SetLastName
                                ]
                            , br [] []
                            , div []
                                [ stylesheet
                                , button [ class "btn btn-link", type_ "button", onClick ClickRegisterUser ]
                                    [ text "S'enregistrer" ]
                                ]
                            ]
                    ]
                ]
    in
    div [ class "jumbotron bg-light text-left" ]
        [ stylesheet, authBoxView ]


showBaseForm : Credential -> Html Msg
showBaseForm credential =
    div []
        [ div []
            [ stylesheet
            , text "Nom d'utilisateur"
            , br [] []
            , viewInput "text" "" credential.username SetUsername
            ]
        , br [] []
        , div []
            [ text "Mot de passe"
            , br [] []
            , viewInput "password" "" credential.password SetPassword
            ]
        , br [] []
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] [ stylesheet ]


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oups, nous n'avons pas trouvé de page à cette adresse." ]


registerUrl : String
registerUrl =
    "http://localhost:3001/users/sign-up"


loginUrl : String
loginUrl =
    "http://localhost:3001/login"



-- Encode user to construct POST request body (for Register and Log In)
-- Decode POST response to get access token


tokenDecoder : Decoder Token
tokenDecoder =
    map6 Token
        (Decode.maybe (Decode.field "timestamp" Decode.string))
        (Decode.maybe (Decode.field "status" Decode.int))
        (Decode.maybe (Decode.field "error" Decode.string))
        (Decode.maybe (Decode.field "message" Decode.string))
        (Decode.maybe (Decode.field "id_token" Decode.string))
        (Decode.maybe (Decode.field "is_admin" Decode.bool))


meDecoder : Decoder User
meDecoder =
    Decode.succeed User
        |> required "id" string
        |> required "person" personDecoder


fetchUserInformation : String -> Cmd Msg
fetchUserInformation token =
    let
        headers =
            [ Http.header "Authorization" token
            ]
    in
    Http.request
        { method = "GET"
        , url = "http://localhost:3001/users/me"
        , expect = Http.expectJson (handleFetchUser token) meDecoder
        , headers = headers
        , body = Http.emptyBody
        , tracker = Nothing
        , timeout = Nothing
        }


handleFetchUser : String -> Result Http.Error User -> Msg
handleFetchUser token result =
    case result of
        Err _ ->
            ErrorFetchingUser "Impossible de charger les données utilisateur"

        Ok user ->
            FetchedUser token user



-- POST register / login request


signUpUserCmd : Model -> String -> Cmd Msg
signUpUserCmd model apiurl =
    let
        body =
            model.credential
                |> userEncoder
                |> Http.jsonBody
    in
    Http.post
        { body = body
        , expect =
            Http.expectJson
                RegisterCompleted
                credentialDecoder
        , url = apiurl
        }


authUserCmd : Model -> String -> Cmd Msg
authUserCmd model apiurl =
    let
        body =
            model.credential
                |> userEncoder
                |> Http.jsonBody
    in
    Http.post
        { body = body
        , expect =
            Http.expectJson
                GetTokenCompleted
                tokenDecoder
        , url = apiurl
        }


getTokenCompleted : Model -> Result Http.Error Token -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newToken ->
            let
                isAdmin =
                    case newToken.is_admin of
                        Just ad ->
                            ad

                        Nothing ->
                            False

                oldCredential =
                    model.credential

                updateToken =
                    { oldCredential | token = Maybe.withDefault "" newToken.id_token, password = "", errorMsg = "" }

                commands =
                    case newToken.id_token of
                        Just tok ->
                            Cmd.batch
                                [ setStorage tok
                                , fetchUserInformation tok
                                , setIsAdminInLocalStorage isAdmin
                                ]

                        Nothing ->
                            Cmd.none

                updateStatus =
                    case newToken.id_token of
                        Just _ ->
                            Unknown

                        Nothing ->
                            LoggedOut
            in
            ( { model | isAdmin = isAdmin, credential = updateToken, auth = updateStatus }, commands )

        Err _ ->
            let
                oldCredential =
                    model.credential

                updateErrorMsg =
                    { oldCredential | errorMsg = "Identification impossible" }
            in
            ( { model | credential = updateErrorMsg }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.planner ) of
        ( ListPlannerMsg subMsg, ListPlanner plannerModel ) ->
            let
                ( updatedPlannerModel, updatedCmd ) =
                    ListPlanners.update subMsg plannerModel
            in
            ( { model | planner = ListPlanner updatedPlannerModel }
            , Cmd.map ListPlannerMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url

                newPlannerRoute =
                    PlannerRoute.parsePlannerUrl url
            in
            ( { model | route = newRoute, plannerRoute = newPlannerRoute }, Cmd.none )
                |> initCurrentPlanner

        ( ViewPlannerMsg subMsg, ViewPlannerById plannerModel ) ->
            let
                ( updatedPlannerModel, updatedCmd ) =
                    ViewPlanner.update subMsg plannerModel
            in
            ( { model | planner = ViewPlannerById updatedPlannerModel }
            , Cmd.map ViewPlannerMsg updatedCmd
            )

        ( SetUsername username, _ ) ->
            let
                oldCredential =
                    model.credential

                updateUsername =
                    { oldCredential | username = username }
            in
            ( { model | credential = updateUsername }, Cmd.none )

        ( SetPassword password, _ ) ->
            let
                oldCredential =
                    model.credential

                updatePassword =
                    { oldCredential | password = password }
            in
            ( { model | credential = updatePassword }, Cmd.none )

        ( SetFirstName firstname, _ ) ->
            let
                oldCredential =
                    model.credential

                newPers =
                    oldCredential.person

                updateFirstName =
                    { newPers | firstname = firstname }

                updateCredential =
                    { oldCredential
                        | person = updateFirstName
                    }
            in
            ( { model | credential = updateCredential }, Cmd.none )

        ( SetLastName lastname, _ ) ->
            let
                oldCredential =
                    model.credential

                newPers =
                    oldCredential.person

                updateLastName =
                    { newPers | lastname = lastname }

                updateCredential =
                    { oldCredential
                        | person = updateLastName
                    }
            in
            ( { model | credential = updateCredential }, Cmd.none )

        ( ClickRegisterUser, _ ) ->
            ( model, signUpUserCmd model registerUrl )

        ( GetTokenCompleted result, _ ) ->
            getTokenCompleted model result

        ( RegisterCompleted (Ok _), _ ) ->
            ( { model | credential = emptyCredential, auth = LoggedOut, register = False }
            , Route.pushUrl Route.Index model.navKey
            )

        ( RegisterCompleted (Err _), _ ) ->
            let
                newCred =
                    emptyCredential

                updateCred =
                    { newCred | errorMsg = "L'utilisateur que vous essayez de créer existe déjà." }
            in
            ( { model | credential = updateCred, auth = LoggedOut, register = False }, Route.pushUrl Route.Index model.navKey )

        ( ClickLogIn, _ ) ->
            ( model, authUserCmd model loginUrl )

        ( FetchedUser tok user, _ ) ->
            ( { model | auth = LoggedIn tok user }, Cmd.batch [ setUserInLocalStorage user, PlannerRoute.pushPlannerUrl PlannerRoute.Planners model.navKey ] )

        ( ErrorFetchingUser error, _ ) ->
            let
                newModel =
                    { model | fetchUserErr = error }
            in
            logout newModel

        ( DisplayRegistrationView, _ ) ->
            ( { model | register = True }, Cmd.none )

        ( LogOut, _ ) ->
            logout model

        ( _, _ ) ->
            ( model, Cmd.none )


logout : Model -> ( Model, Cmd Msg )
logout model =
    let
        cred =
            emptyCredential
    in
    ( { model | credential = cred, auth = LoggedOut }, Cmd.batch [ removeStorage "", removeUserFromLocalStorage emptyUser, removeIsAdminFromLocalStorage False, Route.pushUrl Route.Index model.navKey ] )
