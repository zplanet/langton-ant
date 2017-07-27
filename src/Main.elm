import Html
import Common exposing (..)
import Logic exposing (..)
import Components exposing (render)
import Time exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        getDirection s =
            case s of
                "Left"  -> Left
                "Right" -> Right
                "Top"   -> Top
                _       -> Down
        newModel = 
            case (calc model) of
                Just m  -> m
                Nothing -> { model | isProcessing = False }
    in        
        case msg of
            Toggle          -> ( { model | isProcessing = (not model.isProcessing) }, Cmd.none )
            Reset           -> ( initModel, Cmd.none )
            SetSpeed s      -> ( { model | speed = s }, Cmd.none )
            SetDirection s  -> ( { model | direction = s, currentDirection = (getDirection s) }, Cmd.none )
            Tick _          -> ( newModel, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = 
    if model.isProcessing then every (if model.speed == "Slow" then second else millisecond) Tick 
    else Sub.none

main : Program Never Model Msg
main =
    Html.program
        { init = (initModel, Cmd.none)
        , view = \model -> render model
        , update = update
        , subscriptions = subscriptions
        }
