module Block exposing (Block(..), keypress)

import Keyboard exposing (..)
import Element exposing (..)


type Block
    = Normal (List Element)
    | H1 (List Element)
    | H2 (List Element)
    | H3 (List Element)
    | H4 (List Element)
    | H5 (List Element)
    | P (List Element)


keypress : Char -> Block -> Block
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
