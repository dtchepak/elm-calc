module Calc where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as S
import String exposing (toInt)
import Result exposing (map2)

type alias Model =
    { values : (String, String)
    , result : String
    }
type Action
    = NewValues (String, String)

update : Action -> Model -> Model
update x m =
    case x of
        NewValues (a,b) ->
            { values = (a, b)
            , result = result (toInt a) (toInt b)
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
      [ textInput (\s -> NewValues (s, snd m.values)) address (fst m.values)
      , br [] []
      , textInput (\s -> NewValues (fst m.values, s)) address (snd m.values)
      , br [] []
      , text (m.result)
      ]

main =
  S.start
    { model = { values = ("",""), result = "" }
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

