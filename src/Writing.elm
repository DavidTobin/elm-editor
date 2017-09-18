module Writing exposing (Msg(..), Model, view, init, update, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard exposing (..)
import Block exposing (..)
import Element exposing (..)
import Char
import Key exposing (Key(..), onKeyWithOptions)


type Msg
    = Keypress Key


type alias Model =
    { blocks : List Block
    , cursor : Cursor
    }


init : ( Model, Cmd Msg )
init =
    { blocks = []
    , cursor = Cursor (Normal [ Text "" ]) 0
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Keypress key ->
            let
                ( blocks, cursor ) =
                    Block.updateSelectedBlock key model.cursor model.blocks
            in
                { model | blocks = blocks, cursor = cursor } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []


view : Model -> Html Msg
view model =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        div
            [ contenteditable True
            , styles
            , onKeyWithOptions options (\t -> Keypress t)
            ]
            (List.map (\block -> blockView block) model.blocks)


styles : Html.Attribute Msg
styles =
    style
        [ ( "width", "50%" )
        , ( "height", "200px" )
        , ( "border", "1px solid #dedede" )
        , ( "margin", "10px" )
        , ( "padding", "15px 10px" )
        ]
