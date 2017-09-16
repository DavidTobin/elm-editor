module Demo exposing (..)

import Html exposing (..)
import Editor


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { editor : Editor.Model
    }


type Msg
    = EditorMsg Editor.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( editor, editorMsgs ) =
            Editor.init
    in
        { editor = editor }
            ! [ Cmd.map (\m -> EditorMsg m) editorMsgs ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg msg ->
            let
                ( editor, editorMsgs ) =
                    Editor.update msg model.editor
            in
                { model | editor = editor } ! [ Cmd.map (\m -> EditorMsg m) editorMsgs ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ Html.map (\m -> EditorMsg m) (Editor.view model.editor)
        ]
