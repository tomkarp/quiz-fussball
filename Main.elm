module Main exposing (main)

import Browser
import Html exposing (Html, button, h1, main_, p, text)
import Html.Events exposing (onClick)


type Team
    = Blue
    | Red


type alias Model =
    { blueGoals : Int
    , redGoals : Int
    , position : Int
    }


type Msg
    = Shoot Team


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { blueGoals = 0, redGoals = 0, position = 0 }


stations : Int
stations =
    2


update : Msg -> Model -> Model
update msg model =
    case msg of
        Shoot team ->
            let
                direction =
                    case team of
                        Blue ->
                            1

                        Red ->
                            -1

                position =
                    move direction model.position
            in
            if position > stations then
                { model | blueGoals = model.blueGoals + 1, position = 0 }

            else if position < -stations then
                { model | redGoals = model.redGoals + 1, position = 0 }

            else
                { model | position = position }


move : Int -> Int -> Int
move direction position =
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
        [ h1 [] [ text "Quiz-Fußball" ]
        , p [] [ text ("Stationen bis zum Tor: " ++ String.fromInt stations) ]
        , p [] [ text ("Spielstand: Blau " ++ String.fromInt model.blueGoals ++ " : Rot " ++ String.fromInt model.redGoals) ]
        , p [] [ text ("Ballposition: " ++ String.fromInt model.position) ]
        , button [ onClick (Shoot Blue) ] [ text "Blau schießt" ]
        , button [ onClick (Shoot Red) ] [ text "Rot schießt" ]
        ]
