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
    }


init : Location -> ( Model, Cmd Msg )
init location =
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
                ( loginModel, cmd ) =
                    Login.update msg model.login
            in
                ( { model | login = loginModel }
                , Cmd.map LoginMsg cmd
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


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ onClick (Navigate LeaderBoardPage) ] [ text "Race Results" ]
        , ul []
            [ li []
                [ a [ onClick (Navigate RunnerPage) ]
                    [ text "Link" ]
                ]
            ]
        , ul []
            [ li []
                [ a [ onClick (Navigate LoginPage) ] [ text "Login" ]
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

        "#/runner" ->
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
            "#/runner"

        NotFound ->
            ""


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


main : Program Never Model Msg
main =
    Navigation.program locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
