module Main exposing (..)

import Browser
import Html exposing (Html, text, div, button, p, label, input, form, h1, h2, h3, span)
import Html.Attributes exposing (class, for, id, disabled)
import Html.Events exposing (onClick, onSubmit)
import Html.Events.Extra exposing (onChange)
import String exposing (fromInt)
import List exposing (map, head, length, reverse, filter)
import Set exposing (size)
import Maybe exposing (withDefault)

type alias VoteOption =
    { name : String
    , value : Maybe Int }

type alias CastVote =
    { by : String
    , vote : VoteOption }

type Screen
    = Username
    | Waiting
    | Voting
    | Results

type alias Ticket =
    { id: Maybe String
    , title: String
    , description: Maybe String }

type alias Model =
    { vote: Maybe VoteOption
    , username: Maybe String
    , screen: Screen
    , votes: List CastVote
    , ticket: Maybe Ticket }

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
       , screen = Results
       , votes =
             [{ by = "Laurence"
              , vote = { name = "3"
                       , value = Just 3 }}
             , { by = "Tim"
               , vote = { name = "3"
                        , value = Just 3}}
             , { by = "Eric"
               , vote = { name = "3"
                        , value = Just 3 }}
             , { by = "Sean"
               , vote = { name = "3"
                        , value = Just 3 }}
             ]
       , ticket = Nothing }
           -- Just
           -- { id = Just "sc-12345"
           -- , title = "Run spline manifolds through the turboencabulator"
           -- , description = Just "The original machine has a base-plate of prefabulated amulite, surmounted by a malleable logarithmic casing in such a way that the two spurving bearings were in a direct line with the pentametric fan. The latter consisted simply of six hydrocoptic marzelvanes, so fitted to the ambifacient lunar waneshaft that side fumbling was effectively prevented. The main winding was of the normal lotus-o-delta type placed in panendermic semiboloid slots in the stator, every seventh conductor being connected by a non-reversible tremie pipe to the differential girdlespring on the \"up\" end of the grammeters."
           -- }}

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetUsername username ->
            { model | username = Just username }
        Vote vote ->
            { model | vote = Just vote }
        SetScreen screen ->
            { model | screen = screen }

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

disabledVoteButton : VoteOption -> Html Msg
disabledVoteButton value =
    button [ disabled True
           , class "card card-disabled" ]
           [ text value.name ]

voteButton : VoteOption -> Model -> Html Msg
voteButton value model =
    button [ onClick (Vote value)
           , class ("card " ++ (selectedClass value model)) ]
           [ text value.name ]

renderUsername : Model -> Html Msg
renderUsername model =
    case model.username of
        Just name ->
            span [ class "username" ] [text name]
        Nothing ->
            span [ class "username"] []

header : Model -> Html Msg
header model =
    div [ class "header" ]
        [ h1 [ class "title"]
              [ span [ class "title-emoji" ] [ text "🃏 " ]
              , span [ class "title-words"] [text "Planning Joker"] ]
        , renderUsername model ]

view : Model -> Html Msg
view model =
    div [class "wrapper"]
        [ header model
        , div [class "container"]
            [(screenToView model.screen) model]
        ]

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

renderVote : Maybe VoteOption -> String
renderVote vo =
    case vo of
        Just v ->
            v.name
        Nothing ->
            ""

usernameView : Model -> Html Msg
usernameView model =
    form [ onSubmit (SetScreen Waiting)
         , class "username-form" ]
        [ label [ for "username"
                , class "username-input-label" ] [ text "What's your name?" ]
        , input [ id "username"
                , class "username-input-text"
                , onChange (\s -> SetUsername s)
                ] [ (text << defaultEmpty) model.username ]]

renderTicketId : Maybe String -> Html Msg
renderTicketId ticketId =
    case ticketId of
        Just id ->
            span [class "ticket-id"] [text id]
        Nothing ->
            span [class "ticket-id ticket-id-missing"] []

renderTicketDescription : Maybe String -> Html Msg
renderTicketDescription desc =
    case desc of
        Just description ->
            span [class "ticket-description"] [text description]
        Nothing ->
            span [class "ticket-description ticket-id-missing"] []

renderTicket : Model -> Html Msg
renderTicket model =
    case model.ticket of
        Just ticket ->
            div [ class "voting-on"]
                [ div [ class "ticket" ]
                      [ div [ class "ticket-strap" ]
                            [ h3 [ class "ticket-title" ] [ text ticket.title ]
                            , renderTicketId ticket.id ]
                      , renderTicketDescription ticket.description]]
        Nothing ->
            div [class "ticket ticket-missing"] [text "No ticket selected!"]

waitingView : Model -> Html Msg
waitingView model =
    div [ class "waiting" ]
        [ renderTicket model
        , h2 [ class "waiting-text" ] [ text "Waiting for vote to start..." ]
        , div [] (map disabledVoteButton voteOptions)
        , button [ onClick (SetScreen Voting)] [text "Simulate vote start"]]

votingView : Model -> Html Msg
votingView model =
    div []
        [ renderTicket model
        , div [ class "votes-container" ] (map (\vo -> voteButton vo model) voteOptions)
        , button [ onClick (SetScreen Results)] [text "Simulate vote end"]
        ]

getValue : CastVote -> Int
getValue cv =
    case cv.vote.value of
        Just v ->
            v
        Nothing ->
            999 -- float ? and coffee to the top

sortedCastVotes : List CastVote -> List CastVote
sortedCastVotes cvs = List.sortBy getValue cvs

renderCastVote : CastVote -> Html Msg
renderCastVote vote =
    div [ class "vote" ]
        [ span [ class "vote-voter" ]
               [ text vote.by ]
        , span [ class "vote-name" ]
               [ text vote.vote.name ]]

renderCastVotes : List CastVote -> Html Msg
renderCastVotes cvs =
    let votes = reverse (sortedCastVotes cvs)
    in div [class "votes"] (map renderCastVote votes)

getName : CastVote -> String
getName cv =
    cv.vote.name

hasValue : CastVote -> Bool
hasValue cv =
    case cv.vote.value of
        Just _ ->
            True
        Nothing ->
            False

isConclusive : List CastVote -> Bool
isConclusive cvs =
    hasNUniqueItems 1 (map getName (filter hasValue cvs))

hasNUniqueItems : Int -> List comparable -> Bool
hasNUniqueItems n xs =
    (size (Set.fromList xs)) == n

voteIndex : Int -> Maybe Int
voteIndex v =
    case v of
        0 -> Just 0
        1 -> Just 1
        2 -> Just 2
        5 -> Just 3
        8 -> Just 4
        13 -> Just 5
        20 -> Just 6
        40 -> Just 7
        100 -> Just 8
        _ -> Nothing

noVotesResult : Html Msg
noVotesResult = h1 [ class "cast-vote-result-text" ] [ text "No votes cast!" ]

singleVote : List CastVote -> Bool
singleVote cvs = length cvs == 1

first : List a -> Maybe a
first = head

last : List a -> Maybe a
last = head << reverse

isClose : List CastVote -> Bool
isClose _ = False

voteName : CastVote -> String
voteName cv = cv.vote.name

findCastVoteName : (List CastVote -> Maybe CastVote) -> List CastVote -> String
findCastVoteName f cvs =
    Maybe.withDefault "" (Maybe.map voteName (f cvs))

firstCastVoteName = findCastVoteName first
lastCastVoteName = findCastVoteName last

renderResult : List CastVote -> Html Msg
renderResult cvs =
        case cvs of
            [] -> noVotesResult
            _ ->
                if singleVote cvs then
                        h1 [ class "cast-vote-result-text cast-vote-result-text-conclusive" ] [ text (firstCastVoteName cvs) ]
                else
                    if isConclusive cvs then
                        h1 [ class "cast-vote-result-text cast-vote-result-text-conclusive" ] [ text (firstCastVoteName cvs) ]
                    else if isClose cvs then
                        h1 [ class "cast-vote-result-text cast-vote-result-text-conclusive" ] [ text (lastCastVoteName cvs) ]
                    else
                        h1 [ class "cast-vote-result-text" ] [ text "Try again" ]

        -- Close
        -- Inconclusive

resultsView : Model -> Html Msg
resultsView model =
    div []
        [ renderTicket model
        , div [ class "cast-votes-result" ] [ renderResult (sortedCastVotes model.votes) ]
        , div [ class "cast-votes-container" ] [ renderCastVotes model.votes ]
        , button [ onClick (SetScreen Waiting) ] [ text "Simulate new vote" ]
        ]
