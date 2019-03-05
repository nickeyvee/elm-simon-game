import Browser
import Platform
import Css exposing (display)
import Html exposing (Html, h1, button, div, text, ul, li)
import Html.Styled.Attributes exposing (css, href)
import Html.Events exposing (onClick)
import Process exposing (sleep)
import Random exposing (int, initialSeed, generate)
import Tuple exposing (..)
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
    , current : String
    , running : Bool
    , nextLevel : Bool
    , sequence : List String
    , playerSequence: List String
    , sequenceCopy : List String
    , increment : Int
    , colors : List String
    }





init : () -> (Model, Cmd Msg)
init _ =
  ( { power = False
  , length = 0
  , zone = Time.utc
  , time = Time.millisToPosix 0
  , running = False
  , nextLevel = False
  , current = "0"
  , sequence = []
  , sequenceCopy = []
  , playerSequence = []
  , random = "0"
  , increment = 0
  , colors =
      [ "green"
      , "red"
      , "yellow"
      , "blue"
      ]
  }
  , Cmd.none
  )



type Msg = TogglePower
    | StartSequence
    | ClearSequence
    | NextTick Time.Posix
    | RunGenerator
    | TileSelected String
    | NextLevel Int
    | LoopSequence Time.Posix
    | SaveRandom Int
    -- | AddToSequence Int
    | AdjustTimeZone Time.Zone


-- update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    TileSelected tile ->
      ( case List.head model.sequenceCopy of
          Just string ->
            if lastItem model && string == tile then
              { model | current = string,
                sequenceCopy = dropFirst model,
                nextLevel = True
              }
            else if string == tile then
              { model | current = string,
                sequenceCopy = dropFirst model
              }            
            else
              { model |
                sequenceCopy = model.sequence
              }
          Nothing ->
            model
      ,
        -- if List.length model.sequenceCopy == 0 then
        --   Random.generate NextLevel (Random.int 0 3)
        -- else
        Cmd.none
      )
    StartSequence ->
      ( { model |
        running = not model.running,
        length = model.length + 1
      }
      , Cmd.none
      )
    ClearSequence ->
      ( { model | sequence = [] }
      , Cmd.none
      )  
    NextTick newTime ->
      ( { model |
        time = newTime,
        nextLevel = False
      }
      , Random.generate NextLevel (Random.int 0 3)
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
      , Random.generate NextLevel (Random.int 0 3)
      )
    SaveRandom val -> 
      ( String.fromInt val
        |> (\str -> { model | random = str })
      , Cmd.none
      )
    LoopSequence newTime ->
      ( case getFirst model of
          Just item ->
            { model |
              time = newTime,
              current = item,
              sequenceCopy = dropFirst model
            }
          Nothing ->
            { model |
              sequenceCopy = List.reverse model.sequence,
              running = False
            }
      , Cmd.none
      )
    NextLevel val ->
      ( String.fromInt val
        |> (\a -> a :: model.sequence)
        |> (\list -> { model |
          nextLevel = False,
          sequence = list,
          sequenceCopy = list
        })
      , Cmd.none
      )
  


lastItem : Model -> Bool
lastItem model =
  if List.length (dropFirst model) == 0 then True else False
  



subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
      Time.every 1000 LoopSequence
    else if model.nextLevel then
      Time.every 2000 NextTick
    else
      Sub.none



view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (onOrOff model.power) ]
    , button [ onClick StartSequence ] [ text (startStop model.running)]
    , button [ onClick TogglePower ] [ text (onOrOff model.power) ]
    -- , button [ onClick LoopSequence ] [ text "Loop Through Sequence" ]
    , button [ onClick RunGenerator ] [ text "Simon Says"]
    , button [ onClick ClearSequence ] [ text "Clear Sequence"]
    , h1 [] [ text model.current ]
    , div [] [ text "Sequence Source"]
    , div [] [ sequenceTags model.sequence ]
    , div [] [ text "Sequence Copy"]
    , div [] [ sequenceTags model.sequenceCopy ]
    -- , div [] [ text ]
    , div [] [ text (humanTime model) ]
    , div []
        [ button [ onClick (TileSelected "0") ] [ text "green" ]
        , button [ onClick (TileSelected "1") ] [ text "red" ]
        , button [ onClick (TileSelected "2") ] [ text "yellow" ]
        , button [ onClick (TileSelected "3") ] [ text "blue" ]
        ]
    ]




dropFirst : Model -> List String
dropFirst model =
    List.drop 1 model.sequenceCopy

getFirst : Model -> Maybe String
getFirst model =
    List.head model.sequenceCopy  


humanTime : Model -> String
humanTime model =
    "Seconds : " ++ (String.fromInt (Time.toSecond model.zone model.time))



onOrOff : Bool -> String
onOrOff power =
    if power == True then "On" else "Off"



startStop : Bool -> String
startStop val =
  if val == True then "Running" else "Stopped"


-- maybeCurrent 
-- maybeCurrent model list =
--   case list of
--     Just item ->
--         { model | current = Tuple.first item }
--     Nothing ->
--         model



sequenceTags : List String -> Html Msg
sequenceTags values =
  let
    tags =
      List.map sequenceTag values
  in
  ul [] tags



sequenceTag : String -> Html Msg
sequenceTag str =
  li [] [ text ("Simon says tile " ++ str) ]


 
compare str =
  (\b -> String.fromInt (Tuple.first b) == str)