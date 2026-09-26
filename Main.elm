module Main exposing (main)

import Browser
import Html exposing (Html, article, button, div, footer, header, h1, li, main_, ol, output, section, span, strong, text)
import Html.Attributes exposing (attribute, type_)
import Html.Events exposing (onClick)


type Team
    = Blue
    | Red


type BallPosition
    = Kickoff
    | Station Int


type ShotResult
    = Goal
    | MoveTo BallPosition


type alias Model =
    { blueGoals : Int
    , redGoals : Int
    , stations : Int
    , position : BallPosition
    }


type Msg
    = Score Team
    | SetStations Int
    | Reset


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { blueGoals = 0, redGoals = 0, stations = 2, position = Kickoff }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Score team ->
            let
                direction =
                    if team == Blue then
                        1

                    else
                        -1
            in
            case nextPosition model.stations direction model.position of
                Goal ->
                    case team of
                        Blue ->
                            { model | blueGoals = model.blueGoals + 1, position = Kickoff }

                        Red ->
                            { model | redGoals = model.redGoals + 1, position = Kickoff }

                MoveTo position ->
                    { model | position = position }

        SetStations number ->
            setStationCount number model

        Reset ->
            init


setStationCount : Int -> Model -> Model
setStationCount number model =
    let
        stations =
            max 0 number
    in
    { model
        | stations = stations
        , position =
            if stations == 0 then
                Kickoff

            else
                case model.position of
                    Kickoff ->
                        Kickoff

                    Station position ->
                        Station (clamp (-stations) stations position)
    }


view : Model -> Html Msg
view model =
    main_ []
        [ header [] [ h1 [] [ text "Quiz-Fußball" ] ]
        , section []
            [ scoreCard "Blau" model.blueGoals
            , scoreCard "Rot" model.redGoals
            ]
        , section []
            [ shotControls
            , pitch model
            , shotControls
            ]
        , footer []
            [ div []
                [ span [] [ text "Stationen bis zum Tor" ]
                , button [ type_ "button", onClick (SetStations (model.stations - 1)) ] [ text "−" ]
                , output [] [ text (String.fromInt model.stations) ]
                , button [ type_ "button", onClick (SetStations (model.stations + 1)) ] [ text "+" ]
                ]
            , button [ onClick Reset ] [ text "Spiel zurücksetzen" ]
            ]
        ]


scoreCard : String -> Int -> Html Msg
scoreCard teamName goals =
    article []
        [ div []
            [ strong [] [ text ("Team " ++ teamName) ]
            , output [] [ text (String.fromInt goals) ]
            ]
        ]


shotButton : Team -> String -> Html Msg
shotButton team caption =
    let
        arrow =
            if team == Blue then
                "→"

            else
                "←"
    in
    button [ onClick (Score team), attribute "aria-label" caption ] [ text arrow ]


shotControls : Html Msg
shotControls =
    div []
        [ shotButton Blue "Blau schießt"
        , shotButton Red "Rot schießt"
        ]


nextPosition : Int -> Int -> BallPosition -> ShotResult
nextPosition stations direction position =
    case position of
        Kickoff ->
            if stations == 0 then
                Goal

            else
                MoveTo (Station direction)

        Station current ->
            let
                next =
                    current + direction

                nextStation =
                    if next == 0 then
                        direction

                    else
                        next
            in
            if nextStation > stations || nextStation < -stations then
                Goal

            else
                MoveTo (Station nextStation)


pitch : Model -> Html Msg
pitch model =
    section []
        [ div []
            [ div []
                [ div [] []
                , div [] []
                , div [] []
                , div [] []
                , div [] []
                , div [] []
                ]
            , track model -1
            , track model 1
            , if model.position == Kickoff then
                strong [] [ text "⚽" ]

              else
                text ""
            ]
        ]


track : Model -> Int -> Html Msg
track model direction =
    let
        station index =
            li []
                [ if model.position == Station (direction * index) then
                    strong [] [ text "⚽" ]

                  else
                    span [] []
                ]
    in
    ol [] (List.map station (List.range 1 model.stations))
