module Components exposing (render)

import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Bitwise exposing (and)

renderGrid : Grid -> Html msg
renderGrid grid = 
    let
        cell v = div [ classList [ ("cell", True), ("flip", 0 < (and 1 v)) ] ] [ text (if 0 < (and 2 v) then "a" else " ") ]
        row cs = div [ class "row" ] (List.map (\x -> cell x) cs)
    in
        div [ class "grid" ] (List.map (\x -> row x) grid)

render : Model -> Html Msg
render model =
    let
        { currentLocation, currentDirection, grid, isProcessing, speed, direction } = model
        options s = option [ value s ] [ text s ]
        renderSelect = select [ onInput SetSpeed ] (List.map options speedOptions)
        renderDirection = select [ onInput SetDirection ] (List.map options directionOptions)
    in
        div []
            [ div [ class "container" ] [ h1 [] [ text "Langton's ant" ], a [ href "https://en.wikipedia.org/wiki/Langton%27s_ant", class "info" ] [ text "more info" ] ]
            , div [] [ text "Speed: ", (if isProcessing then (text speed) else renderSelect) ]
            , div [] [ text "Direction: ", (if isProcessing then (text direction) else renderDirection) ]
            , button [ onClick Toggle ] [ text (if isProcessing then "Stop" else "Start") ]
            , button [ onClick Reset ] [ text "Reset" ]
            , div [] [ text ("Current Location: " ++ (toString currentLocation)) ]
            , div [] [ text ("Current Direction: " ++ (toString currentDirection)) ]
            , renderGrid grid
            ]
