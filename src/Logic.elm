module Logic exposing (..)

import Common exposing (..)
import Bitwise exposing (and, or)

calcLocation : Direction -> Location -> Location
calcLocation direction (x, y) = 
    case direction of
        Left    -> (x - 1, y)
        Right   -> (x + 1, y)
        Top     -> (x, y - 1)
        Down    -> (x, y + 1)

calcDirection : Direction -> Location -> Grid -> Maybe Direction
calcDirection direction (x, y) grid =
    let
        getDirection b =
            case (direction, b) of
                (Left,   True)    -> Down
                (Left,   False)   -> Top
                (Right,  True)    -> Top
                (Right,  False)   -> Down
                (Top,    True)    -> Left
                (Top,    False)   -> Right
                (Down,   True)    -> Right
                (Down,   False)   -> Left
    in
        grid 
        |> List.drop y |> List.head 
        |> Maybe.andThen (\row -> row |> List.drop x |> List.head)
        |> Maybe.map (\c -> 0 < (and 1 c))
        |> Maybe.map (\b -> getDirection b)

updateGrid : Location -> Location -> Grid -> Grid
updateGrid (currX, currY) (nextX, nextY) grid =
    grid
    |> List.indexedMap (\y -> \row -> 
        row 
        |> List.indexedMap (\x -> \cell ->
            if currX == x && currY == y then
                if 0 < (and 1 cell) then 0 else 1
            else
                if nextX == x && nextY == y then (or 2 cell) else cell
        )
    )

calc : Model -> Maybe Model
calc model =
    let
        { currentLocation, currentDirection, grid, isProcessing, speed, direction } = model
    in
        Maybe.andThen
            (\nextDirection ->
                let
                    (nextX, nextY) = calcLocation nextDirection currentLocation
                in
                    if nextX < 0 || nextY < 0 || gridSize <= nextX || gridSize <= nextY then 
                        Nothing
                    else
                        Just (Model (nextX, nextY) nextDirection (updateGrid currentLocation (nextX, nextY) grid) isProcessing speed direction)
            )
            (calcDirection currentDirection currentLocation grid)
