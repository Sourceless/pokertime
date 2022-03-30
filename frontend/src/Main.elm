module Main exposing (..)

import Browser
import Html exposing (Html, text, div, button, p)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import List exposing (map)

type alias VoteOption =
    { name : String
    , value : Maybe Int }

type alias Model =
    { vote: Maybe VoteOption }

type Msg =
    Vote VoteOption

main = Browser.sandbox
       { init = init
       , update = update
       , view = view
       }

init : Model
init = { vote = Nothing }

update : Msg -> Model -> Model
update msg model =
    case msg of
        Vote vote ->
            { model | vote = Just vote }

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
        [div [] (map (\vo -> voteButton vo model) voteOptions) ]
