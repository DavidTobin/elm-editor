module Writing exposing (Msg(..), Model, view, init, update, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard exposing (..)
import Block exposing (..)
import Element exposing (..)
import Helpers exposing (..)
import Char
import Selection exposing (Cursor(..))


type Msg
    = Keypress KeyCode


type alias Model =
    { blocks : List Block
    , cursor : Cursor
    }


init : ( Model, Cmd Msg )
init =
    { blocks = []
    , cursor = NoSelection
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Keypress key ->
            { model | blocks = updateSelectedBlock (Char.fromCode key) model.blocks } ! []


updateSelectedBlock : Char -> List Block -> List Block
updateSelectedBlock key blocks =
    case blocks of
        [] ->
            [ Normal [ Text (String.fromChar key) ] ]

        _ ->
            List.map (\block -> Block.keypress key block) blocks


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []


view : Model -> Html Msg
view model =
    div
        [ contenteditable True
        , styles
        , Helpers.onKeyDown Keypress
        ]
        (List.map (\block -> blockView block) model.blocks)


blockView : Block -> Html Msg
blockView block =
    let
        mapElements elements =
            List.map (\element -> elementView element) elements
    in
        case block of
            Normal elements ->
                div [ blockStyles ] <| mapElements elements

            H1 elements ->
                h1 [ blockStyles ] <| mapElements elements

            H2 elements ->
                h2 [ blockStyles ] <| mapElements elements

            H3 elements ->
                h3 [ blockStyles ] <| mapElements elements

            H4 elements ->
                h4 [ blockStyles ] <| mapElements elements

            H5 elements ->
                h5 [ blockStyles ] <| mapElements elements

            P elements ->
                p [ blockStyles ] <| mapElements elements


elementView : Element -> Html Msg
elementView element =
    case element of
        Text t ->
            text t

        Bold el ->
            strong [] [ elementView el ]

        Italics el ->
            em [] [ elementView el ]


styles : Html.Attribute Msg
styles =
    style
        [ ( "width", "50%" )
        , ( "height", "200px" )
        , ( "border", "1px solid #dedede" )
        , ( "margin", "10px" )
        , ( "padding", "15px 10px" )
        ]


blockStyles : Html.Attribute Msg
blockStyles =
    style
        [ ( "padding", "5px 0" )
        ]
