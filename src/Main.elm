import Browser
import Platform
import Css exposing (..)
import Html.Styled exposing (..)
-- import Html exposing (Html, h1, button, div, text, ul, li)
import Html.Styled.Attributes exposing (css, href)
-- import Html.Styled.toUnstyled
import Html.Styled.Events exposing (onClick)
import Process exposing (sleep)
import Random exposing (int, initialSeed, generate)
import Tuple exposing (..)
import Time exposing (every)


main =
  Browser.element
    { init = init
    , update = update
    , view = view >> toUnstyled
    , subscriptions = subscriptions
    }



type alias Model =
    { power : Bool
    , length : Int
    , zone : Time.Zone
    , time : Time.Posix
    , random : String
    , current : String
    , displaySequence : Bool
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
  , displaySequence = False
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
    | StartGame
    | StartSequence
    | ClearSequence
    | NextTick Time.Posix
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
                displaySequence = True,
                nextLevel = True
              }
            else if string == tile then
              { model | current = string,
                sequenceCopy = dropFirst model
              }            
            else
              { model | displaySequence = True,
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
        displaySequence = not model.displaySequence,
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
    StartGame ->
      ( { model | nextLevel = True, displaySequence = True }
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
              sequenceCopy = model.sequence,
              displaySequence = False
            }
      , Cmd.none
      )
    NextLevel val ->
      ( String.fromInt val
        |> (\a -> a :: (List.reverse model.sequence))
        |> List.reverse
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
    if model.displaySequence then
      Time.every 1000 LoopSequence
    else if model.nextLevel then
      Time.every 0 NextTick
    else
      Sub.none



-- buttons : Model -> Html.Styled Msg
-- button buttons =
--   div [] buttons


view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (onOrOff model.power) ]
    , button [ onClick StartSequence ] [ text (startStop model.displaySequence)]
    , button [ onClick TogglePower ] [ text (onOrOff model.power) ]
    , button [ onClick StartGame ] [ text "Start Game" ]
    -- , button [ onClick RunGenerator ] [ text "Simon Says"]
    , button [ onClick ClearSequence ] [ text "Clear Sequence"]
    , h1 [] [ text model.current ]
    , div [] [ text "Sequence Source"]
    , div [] [ sequenceTags model.sequence ]
    , div [] [ text "Sequence Copy"]
    , div [] [ sequenceTags model.sequenceCopy ]
    , circleChasis [
          div [ css
            [ display block
            , borderRadius (px 500)
            , position relative
            , overflow hidden
            , margin auto
            , height (px 420)
            , width (px 420)
            ]
          ]
          [ controlPanel
              [ div [ css
                  [ width (px 175)
                  , margin (px 17)
                  , height (px 175)
                  , borderRadius (px 500)
                  , backgroundColor (hex "#ffffff")
                  ]
              ] []
            ]
          , simonRow
              [ simonTile "1" "00A74A"
              , simonTile "2" "9F0F17"
              ]
          , simonRow
              [ simonTile "3" "CCA707"
              , simonTile "4" "094A8F"
              ]          
          ]
          , div [ css
              [ position absolute
              , borderRadius (px 100)
              ]
            ] []
        ] 
    ]


controlPanel : List (Html Msg) -> Html Msg
controlPanel element =
    div [ css
          [ position absolute
          , width (px 210)
          , height (px 210)
          , top (px 100)
          , left (px 105)
          , margin auto
          , display block
          , borderRadius (px 500)
          , backgroundColor (hex "#333333")
          ]
        ] element


circleChasis : List (Html Msg) -> Html Msg
circleChasis element =
    div [ css
          [ position relative
          , padding (px 18)
          , display block
          , borderRadius (px 500)
          , backgroundColor (hex "#333333")
          , width (px 420)
          , height (px 420)
          ]
        ] element


simonTile : String -> String -> Html Msg
simonTile index color =
    button [ css
        [ width (px 200)
        , border (px 0)
        , height (px 200)
        , display block
        , padding (px 0)
        , outline none
        , backgroundColor (hex color)
        ]
        , onClick (TileSelected index)
        ] [ ]


simonRow : List (Html Msg) -> Html Msg
simonRow elements =
    div [ css
          [ Css.displayFlex
          , flexWrap wrap
          , height (px 200)
          , justifyContent spaceBetween
          , marginBottom (px 20)
          ]
        ] elements


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