module Element exposing (Element(..), keypress, elementView)

import Html exposing (..)
import Key exposing (Key(..))


type Element
    = Bold Element
    | Italics Element
    | Text String



-- State functions


keypress : Key -> Element -> Element
keypress key element =
    case element of
        Text text ->
            case key of
                Key k ->
                    Text (text ++ k)

                _ ->
                    Text text

        Bold el ->
            Bold (keypress key el)

        Italics el ->
            Italics (keypress key el)



-- Views


elementView : Element -> Html msg
elementView element =
    case element of
        Text t ->
            text t

        Bold el ->
            strong [] [ elementView el ]

        Italics el ->
            em [] [ elementView el ]
