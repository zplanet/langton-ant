module Common exposing (..)

import Time exposing (Time)

type Direction = Left | Right | Top | Down
type alias Location = (Int, Int)
type alias Grid = List (List Int)

type Msg = Toggle | Reset | Tick Time | SetSpeed String | SetDirection String

type alias Model = 
    { currentLocation : Location
    , currentDirection : Direction
    , grid : Grid
    , isProcessing: Bool
    , speed: String
    , direction: String
    }

speedOptions : List String
speedOptions = [ "Slow", "Fast" ]

directionOptions : List String
directionOptions = [ "Left", "Right", "Top", "Down" ]

gridSize : Int
gridSize = 50

center : Int
center = gridSize // 2

indexes : List Int
indexes = List.range 0 (gridSize - 1)

initLocation : (Int, Int)
initLocation = (center, center)

initGrid : List (List Int)
initGrid = (List.map (\y -> List.map (\x -> if x == center && y == center then 2 else 0) indexes) indexes)

initModel : Model
initModel = Model initLocation Left initGrid False "Slow" "Left"