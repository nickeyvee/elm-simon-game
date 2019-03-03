import Browser
import Platform
import Html exposing (Html, button, div, text, ul, li)
import Html.Events exposing (onClick)
import Process exposing (sleep)
import Random exposing (int, initialSeed, generate)
import Time exposing (every)


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



type alias Model =
    { power : Bool
    , length : Int
    , zone : Time.Zone
    , time : Time.Posix
    , random : String
    , running : Bool
    , sequenceCopy : List String
    , sequence : List String
    , increment : Int
    }



init : () -> (Model, Cmd Msg)
init _ =
  ( { power = False
  , length = 0
  , zone = Time.utc
  , time = Time.millisToPosix 0
  , running = False
  , sequenceCopy = []
  , random = "0"
  , sequence = []
  , increment = 0
  }
  , Cmd.none
  )



type Msg = TogglePower
    | StartSequence
    | ClearSequence
    | Tick Time.Posix
    | RunGenerator
    | SaveRandom Int
    | AddToSequence Int
    | AdjustTimeZone Time.Zone


-- update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    StartSequence ->
      ( { model | running = not model.running
      -- | sequence = (\_ -> )
      }
      , Cmd.none
      )
    ClearSequence ->
      ( { model | sequenceCopy = [] }
      , Cmd.none
      )
    Tick newTime ->
      ( { model | time = newTime }
      , Random.generate AddToSequence (Random.int 0 3)
      )
    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )
    TogglePower ->
      ( { model | power = not model.power }
      , Cmd.none
      )
    RunGenerator ->
      ( model
      , Random.generate AddToSequence (Random.int 0 3)
      )
    SaveRandom val -> 
      ( String.fromInt val
        |> (\str -> { model | random = str })
      , Cmd.none
      )
    AddToSequence val ->
      ( String.fromInt val
        |> (\a -> a :: model.sequenceCopy)
        |> (\li -> { model | sequenceCopy = li })
      , Cmd.none
      )
    


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
      Time.every 1000 Tick
    else
      Sub.none



view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (onOrOff model.power) ]
    , button [ onClick StartSequence ] [ text (startStop model.running)]
    , button [ onClick TogglePower ] [ text (onOrOff model.power) ]
    , button [ onClick RunGenerator ] [ text "Simon Says"]
    , button [ onClick ClearSequence ] [ text "Clear Sequence"]
    , div [] [ text "Sequence Copy"]
    , div [] [ sequenceTags model.sequenceCopy ]
    , div [] [ text "Sequence Source"]
    , div [] [ sequenceTags model.sequence ]
    -- , div [] [ text ]
    , div [] [ text (humanTime model) ]
    ]



humanTime : Model -> String
humanTime model =
    "Seconds : " ++ (String.fromInt (Time.toSecond model.zone model.time))



onOrOff : Bool -> String
onOrOff power =
    if power == True then "On" else "Off"



startStop : Bool -> String
startStop val =
  if val == True then "Running" else "Stopped"



sequenceTags : List String -> Html Msg
sequenceTags values =
  let
    tags =
      List.map sequenceTag values
  in
  ul [] tags


sequenceTag : String -> Html Msg
sequenceTag str =
  li [ ] [ text ("Simon says tile " ++ str) ]