module Calc where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as S
import String exposing (toInt)
import Result exposing (map2)

type alias Model =
    { firstValue  : String
    , secondValue : String
    , result      : String
    }
type Action
    = UpdateFirst String
    | UpdateSecond String

update : Action -> Model -> Model
update a m =
    case a of
        UpdateFirst s ->
        { m | firstValue <- s
            , result <- result (toInt s) (toInt m.secondValue)
        }
        UpdateSecond s ->
        { m | secondValue <- s
            , result <- result (toInt m.firstValue) (toInt s) 
        }

result : Result String Int -> Result String Int -> String
result a b =
    map2 (+) a b
    |> either toString identity

textInput : (String -> Action) -> Signal.Address Action -> String -> Html
textInput act address initial =
      input
          [ value initial
          , on "input" targetValue (Signal.message address << act)
          ]
          []

view : Signal.Address Action -> Model -> Html
view address m = 
    div []
      [ textInput UpdateFirst address m.firstValue
      , br [] []
      , textInput UpdateSecond address m.secondValue
      , br [] []
      , text (m.result)
      ]

main =
  S.start
    { model = { firstValue = "", secondValue = "", result = "" }
    , update = update
    , view = view
    }


either : (a -> b) -> (e -> b) -> Result e a -> b
either onSuccess onFail result =
        case result of
            Ok a ->
                onSuccess a
            Err b ->
                onFail b

