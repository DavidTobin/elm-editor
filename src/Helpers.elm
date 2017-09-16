module Helpers exposing (..)

import Json.Decode as Json
import Html.Events exposing (..)
import Html exposing (..)
import Keyboard exposing (..)


onKeyDown : (KeyCode -> msg) -> Html.Attribute msg
onKeyDown tagger =
    let
        options =
            { stopPropagation = True
            , preventDefault = True
            }
    in
        onWithOptions "keydown" options (Json.map tagger keyCode)
