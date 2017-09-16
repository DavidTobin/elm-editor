module Element exposing (Element(..), keypress)


type Element
    = Bold Element
    | Italics Element
    | Text String


keypress : Char -> Element -> Element
keypress char element =
    case element of
        Text text ->
            Text (text ++ String.fromChar char)

        Bold el ->
            Bold (keypress char el)

        Italics el ->
            Italics (keypress char el)
