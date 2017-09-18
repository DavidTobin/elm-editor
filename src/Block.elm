module Block exposing (Block(..), Cursor(..), keypress, updateSelectedBlock, blockView)

import Keyboard exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Key exposing (Key(..))


type Cursor
    = Cursor Block Int


type Block
    = Normal (List Element)
    | H1 (List Element)
    | H2 (List Element)
    | H3 (List Element)
    | H4 (List Element)
    | H5 (List Element)
    | P (List Element)



-- State functions


keypress : Key -> Block -> Block
keypress key block =
    let
        updateElements =
            List.map (\element -> Element.keypress key element)
    in
        case block of
            Normal elements ->
                Normal (updateElements elements)

            H1 elements ->
                Normal (updateElements elements)

            H2 elements ->
                H2 (updateElements elements)

            H3 elements ->
                H3 (updateElements elements)

            H4 elements ->
                H4 (updateElements elements)

            H5 elements ->
                H5 (updateElements elements)

            P elements ->
                P (updateElements elements)


updateSelectedBlock : Key -> Cursor -> List Block -> ( List Block, Cursor )
updateSelectedBlock key (Cursor cursorBlock cursorPosition) blocks =
    case blocks of
        [] ->
            case key of
                Key k ->
                    let
                        element =
                            Text k

                        block =
                            Normal [ element ]
                    in
                        ( [ block ], Cursor block 0 )

                _ ->
                    let
                        element =
                            Text ""

                        block =
                            Normal [ element ]
                    in
                        ( [ block ], Cursor block 0 )

        _ ->
            case key of
                Enter ->
                    let
                        element =
                            Text ""

                        block =
                            Normal [ element ]

                        cursor =
                            Cursor block 0
                    in
                        ( (blocks ++ [ block ]), cursor )

                _ ->
                    let
                        updatedBlock =
                            keypress key cursorBlock

                        updatedBlocks =
                            List.map
                                (\block ->
                                    if block == cursorBlock then
                                        updatedBlock
                                    else
                                        block
                                )
                                blocks
                    in
                        ( updatedBlocks, Cursor updatedBlock (cursorPosition + 1) )



-- Views


blockView : Block -> Html msg
blockView block =
    let
        mapElements elements =
            List.map (\element -> elementView element) elements
    in
        case block of
            Normal elements ->
                div [ styles ] <| mapElements elements

            H1 elements ->
                h1 [ styles ] <| mapElements elements

            H2 elements ->
                h2 [ styles ] <| mapElements elements

            H3 elements ->
                h3 [ styles ] <| mapElements elements

            H4 elements ->
                h4 [ styles ] <| mapElements elements

            H5 elements ->
                h5 [ styles ] <| mapElements elements

            P elements ->
                p [ styles ] <| mapElements elements



-- Styles


styles : Html.Attribute msg
styles =
    style
        [ ( "padding", "5px 0" )
        ]
