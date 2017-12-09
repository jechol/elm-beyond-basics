port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import LeaderBoard
import Login
import Runner


-- model


type alias Model =
    { page : Page
    , leaderBoard : LeaderBoard.Model
    , login : Login.Model
    , runner : Runner.Model
    , token : Maybe String
    , loggedIn : Bool
    }


type Page
    = NotFound
    | LeaderBoardPage
    | LoginPage
    | RunnerPage


initModel : Model
initModel =
    { page = NotFound
    , leaderBoard = LeaderBoard.initModel
    , login = Login.initModel
    , runner = Runner.initModel
    , token = Nothing
    , loggedIn = False
    }


type alias Flags =
    { token : Maybe String }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        page =
            hashToPage location.hash

        ( leaderBoardInitModel, leaderBoardCmd ) =
            LeaderBoard.init

        ( loginInitModel, loginCmd ) =
            Login.init

        ( runnerInitModel, runnerCmd ) =
            Runner.init

        initModel =
            { page = page
            , leaderBoard = leaderBoardInitModel
            , login = loginInitModel
            , runner = runnerInitModel
            , token = Debug.log "SavedToken" flags.token
            , loggedIn = flags.token /= Nothing
            }

        cmds =
            Cmd.batch
                [ Cmd.map LeaderBoardMsg leaderBoardCmd
                , Cmd.map LoginMsg loginCmd
                , Cmd.map RunnerMsg runnerCmd
                ]
    in
        ( initModel, cmds )



-- update


type Msg
    = Navigate Page
    | ChangePage Page
    | LeaderBoardMsg LeaderBoard.Msg
    | LoginMsg Login.Msg
    | RunnerMsg Runner.Msg
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Navigation.newUrl <| pageToHash page )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        LeaderBoardMsg msg ->
            let
                ( leaderBoardModel, cmd ) =
                    LeaderBoard.update msg model.leaderBoard
            in
                ( { model | leaderBoard = leaderBoardModel }
                , Cmd.map LeaderBoardMsg cmd
                )

        LoginMsg msg ->
            let
                ( loginModel, cmd, token ) =
                    Login.update msg model.login

                loggedIn =
                    token /= Nothing

                saveTokenCmd =
                    case Debug.log "Token" token of
                        Just jwt ->
                            saveToken jwt

                        Nothing ->
                            Cmd.none
            in
                ( { model
                    | login = loginModel
                    , token = token
                    , loggedIn = loggedIn
                  }
                , Cmd.batch
                    [ Cmd.map LoginMsg cmd
                    , saveTokenCmd
                    ]
                )

        Logout ->
            ( { model
                | loggedIn = False
                , token = Nothing
              }
            , deleteToken ()
            )

        RunnerMsg msg ->
            let
                ( runnerModel, cmd ) =
                    Runner.update msg model.runner
            in
                ( { model | runner = runnerModel }
                , Cmd.map RunnerMsg cmd
                )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                LeaderBoardPage ->
                    Html.map LeaderBoardMsg (LeaderBoard.view model.leaderBoard)

                LoginPage ->
                    Html.map LoginMsg (Login.view model.login)

                RunnerPage ->
                    Html.map RunnerMsg (Runner.view model.runner)

                NotFound ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
        div []
            [ pageHeader model
            , page
            ]


authHeaderView : Model -> Html Msg
authHeaderView model =
    if model.loggedIn then
        a [ onClick Logout ] [ text "Logout" ]
    else
        a [ onClick (Navigate LoginPage) ] [ text "Loin" ]


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ onClick (Navigate LeaderBoardPage) ] [ text "Race Results" ]
        , ul []
            [ li []
                [ a [ onClick (Navigate RunnerPage) ]
                    [ text "Add Runner" ]
                ]
            ]
        , ul []
            [ li []
                [ authHeaderView model
                ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        leaderBoardSub =
            LeaderBoard.subscriptions model.leaderBoard
    in
        Sub.batch
            [ Sub.map LeaderBoardMsg leaderBoardSub
            ]


hashToPage : String -> Page
hashToPage hash =
    case (Debug.log "Hash" hash) of
        "#/" ->
            LeaderBoardPage

        "" ->
            LeaderBoardPage

        "#/login" ->
            LoginPage

        "#/add" ->
            RunnerPage

        _ ->
            NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        LeaderBoardPage ->
            "#/"

        LoginPage ->
            "#/login"

        RunnerPage ->
            "#/add"

        NotFound ->
            ""


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


main : Program Flags Model Msg
main =
    Navigation.programWithFlags locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port saveToken : String -> Cmd msg


port deleteToken : () -> Cmd msg
