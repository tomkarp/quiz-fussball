module Main exposing (main)

import Browser
import Html exposing (Html, article, button, div, footer, header, h1, li, main_, ol, output, section, span, strong, text)
import Html.Attributes exposing (attribute, type_)
import Html.Events exposing (onClick)


type Team
    = Blue
    | Red


type alias Model =
    { blueGoals : Int
    , redGoals : Int
    , stations : Int
    , position : Int
    }


type Msg
    = Shoot Team
    | SetStations Int
    | Reset


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { blueGoals = 0, redGoals = 0, stations = 2, position = 0 }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Shoot team ->
            let
                direction =
                    if team == Blue then
                        1

                    else
                        -1

                position =
                    nextPosition direction model.position
            in
            if position > model.stations then
                { model | blueGoals = model.blueGoals + 1, position = 0 }

            else if position < -model.stations then
                { model | redGoals = model.redGoals + 1, position = 0 }

            else
                { model | position = position }

        SetStations number ->
            let
                stations =
                    max 0 number
            in
            { model
                | stations = stations
                , position = clamp (-stations) stations model.position
            }

        Reset ->
            init


-- 0 ist der Anspielpunkt. Im laufenden Spiel wird er übersprungen.
nextPosition : Int -> Int -> Int
nextPosition direction position =
    let
        next =
            position + direction
    in
    if next == 0 then
        direction

    else
        next


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


shotControls : Html Msg
shotControls =
    div []
        [ button [ onClick (Shoot Blue), attribute "aria-label" "Blau schießt" ] [ text "→" ]
        , button [ onClick (Shoot Red), attribute "aria-label" "Rot schießt" ] [ text "←" ]
        ]


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
            , if model.position == 0 then
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
                [ if model.position == direction * index then
                    strong [] [ text "⚽" ]

                  else
                    span [] []
                ]
    in
    ol [] (List.map station (List.range 1 model.stations))
