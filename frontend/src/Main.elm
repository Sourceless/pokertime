module Main exposing (..)

import Browser
import Html exposing (Html, text, div, button, p, label, input, form)
import Html.Attributes exposing (class, for, id)
import Html.Events exposing (onClick, onSubmit)
import Html.Events.Extra exposing (onChange)
import String exposing (fromInt)
import List exposing (map)

type alias VoteOption =
    { name : String
    , value : Maybe Int }

type Screen
    = Username
    | Waiting
    | Voting
    | Results

type alias Model =
    { vote: Maybe VoteOption
    , username: Maybe String
    , screen: Screen }

type Msg
    = Vote VoteOption
    | SetUsername String
    | SetScreen Screen

main = Browser.sandbox
       { init = init
       , update = update
       , view = view
       }

init : Model
init = { vote = Nothing
       , username = Nothing
       , screen = Username }

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetUsername username ->
            { model | username = Just username }
        Vote vote ->
            { model | vote = Just vote }
        SetScreen screen ->
            case model.username of
                Just _ ->
                    { model | screen = screen }
                Nothing ->
                    model

numericVoteOption : Int -> VoteOption
numericVoteOption n =
    { name = fromInt n
    , value = Just n }

voteOptions : List VoteOption
voteOptions = [{ name = "?", value = Nothing}]
              ++ map numericVoteOption [0, 1, 2, 3, 5, 8, 13, 20, 40, 100]
              ++ [{ name = "☕", value = Nothing }]

isSelected : VoteOption -> Model -> Bool
isSelected vo model =
    case model.vote of
        Just v ->
            v.name == vo.name
        Nothing ->
            False

selectedClass : VoteOption -> Model -> String
selectedClass vo m =
    if isSelected vo m then
        "card-selected"
    else
        ""

voteButton : VoteOption -> Model -> Html Msg
voteButton value model =
    button [ onClick (Vote value)
           , class ("card " ++ (selectedClass value model)) ]
           [ text value.name ]

view : Model -> Html Msg
view model =
    div [class "container"]
        [(screenToView model.screen) model]

screenToView : Screen -> (Model -> Html Msg)
screenToView screen =
    case screen of
        Username ->
            usernameView
        Waiting ->
            waitingView
        Voting ->
            votingView
        Results ->
            resultsView

defaultEmpty : Maybe String -> String
defaultEmpty ms =
    case ms of
        Just s ->
            s
        Nothing ->
            ""

usernameView : Model -> Html Msg
usernameView model =
    form [onSubmit (SetScreen Waiting)]
        [ label [ for "username"
                , class "input-label" ] [ text "What's your name?" ]
        , input [ id "username"
                , class "input-text"
                , onChange (\s -> SetUsername s)
                ] [ (text << defaultEmpty) model.username ]]

waitingView : Model -> Html Msg
waitingView model =
    p [] [button [ onClick (SetScreen Voting)] [text "Simulate vote start"]]

votingView : Model -> Html Msg
votingView model =
    div [] (map (\vo -> voteButton vo model) voteOptions)

resultsView : Model -> Html Msg
resultsView model =
    p [] [text "results"]
