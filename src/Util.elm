module Util exposing (toJson)

import Json.Encode exposing (..)
import Writing exposing (..)
import Block exposing (..)
import Element exposing (..)


-- Json Helpers


toJson : Model -> String
toJson model =
    object
        [ ( "writing", encodeBlocks model.blocks )
        ]
        |> encode 2


encodeBlocks : List Block -> Value
encodeBlocks blocks =
    list
        (List.map (\block -> encodeBlock block) blocks)


encodeBlock : Block -> Value
encodeBlock block =
    let
        ( t, e ) =
            case block of
                Normal elements ->
                    ( "normal", encodeElements elements )

                H1 elements ->
                    ( "h1", encodeElements elements )

                H2 elements ->
                    ( "h2", encodeElements elements )

                H3 elements ->
                    ( "h3", encodeElements elements )

                H4 elements ->
                    ( "h4", encodeElements elements )

                H5 elements ->
                    ( "h5", encodeElements elements )

                P elements ->
                    ( "p", encodeElements elements )
    in
        object
            [ ( "t", string t )
            , ( "els", e )
            ]


encodeElements : List Element -> Value
encodeElements elements =
    list
        (List.map (\element -> encodeElement element) elements)


encodeElement : Element -> Value
encodeElement element =
    case element of
        Text t ->
            object [ ( "t", string "text" ), ( "val", string t ) ]

        Bold e ->
            object [ ( "t", string "bold" ), ( "val", (encodeElement e) ) ]

        Italics e ->
            object [ ( "t", string "italic" ), ( "val", (encodeElement e) ) ]
