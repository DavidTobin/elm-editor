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

                        updateBlocks blocks_ =
                            case blocks_ of
                                [] ->
                                    []

                                a :: [] ->
                                    [ a, block ]

                                a :: b :: rest ->
                                    if a == cursorBlock then
                                        [ a, block ] ++ [ b ] ++ rest
                                    else
                                        [ a ] ++ (updateBlocks (b :: rest))
                    in
                        ( updateBlocks blocks, cursor )

                ArrowRight ->
                    let
                        textSize =
                            cursorBlock
                                |> toText
                                |> String.length

                        cursorPosition_ =
                            if cursorPosition < textSize then
                                cursorPosition + 1
                            else
                                cursorPosition
                    in
                        ( blocks, Cursor cursorBlock cursorPosition_ )

                ArrowLeft ->
                    let
                        cursorPosition_ =
                            if cursorPosition > 0 then
                                cursorPosition - 1
                            else
                                0
                    in
                        ( blocks, Cursor cursorBlock cursorPosition_ )

                ArrowUp ->
                    let
                        prevItem blocks_ =
                            case blocks_ of
                                [] ->
                                    cursorBlock

                                x :: [] ->
                                    cursorBlock

                                x :: y :: rest ->
                                    if y == cursorBlock then
                                        x
                                    else
                                        prevItem (y :: rest)
                    in
                        ( blocks, Cursor (prevItem blocks) 0 )

                ArrowDown ->
                    let
                        nextItem blocks_ =
                            case blocks_ of
                                [] ->
                                    cursorBlock

                                x :: [] ->
                                    cursorBlock

                                x :: y :: rest ->
                                    if x == cursorBlock then
                                        y
                                    else
                                        nextItem (y :: rest)
                    in
                        ( blocks, Cursor (nextItem blocks) 0 )

                Backspace ->
                    let
                        updatedBlock =
                            backspace (Cursor cursorBlock cursorPosition) cursorBlock

                        cursor =
                            Cursor updatedBlock (cursorPosition - 1)

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
                        ( updatedBlocks, cursor )

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


backspace : Cursor -> Block -> Block
backspace (Cursor cursorBlock cursorPosition) block =
    let
        backspace_ : List Element -> List Element
        backspace_ elements =
            case elements of
                [] ->
                    elements

                a :: [] ->
                    [ a ]

                a :: b :: rest ->
                    [ a ] ++ (backspace_ ([ b ] ++ rest))
    in
        case cursorPosition of
            0 ->
                block

            _ ->
                case block of
                    Normal elements ->
                        Normal (backspace_ elements)

                    H1 elements ->
                        Normal (backspace_ elements)

                    H2 elements ->
                        H2 (backspace_ elements)

                    H3 elements ->
                        H3 (backspace_ elements)

                    H4 elements ->
                        H4 (backspace_ elements)

                    H5 elements ->
                        H5 (backspace_ elements)

                    P elements ->
                        P (backspace_ elements)



-- Helpers


blockElements : Block -> List Element
blockElements block =
    case block of
        Normal elements ->
            elements

        P elements ->
            elements

        H1 elements ->
            elements

        H2 elements ->
            elements

        H3 elements ->
            elements

        H4 elements ->
            elements

        H5 elements ->
            elements


extractText : Element -> String
extractText element =
    case element of
        Text t ->
            t

        Bold el ->
            extractText el

        Italics el ->
            extractText el


toText : Block -> String
toText block =
    block
        |> blockElements
        |> List.map extractText
        |> String.join ""



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
