module Editor exposing (Msg, Model, view, init, update, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Writing
import Util exposing (toJson)


type Msg
    = WritingMsg Writing.Msg


type alias Model =
    { writing : Writing.Model }


init : ( Model, Cmd Msg )
init =
    let
        ( writing, writingMsg ) =
            Writing.init
    in
        { writing = writing }
            ! [ Cmd.map (\m -> WritingMsg m) writingMsg
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WritingMsg msg ->
            let
                ( writing, writingMsg ) =
                    Writing.update msg model.writing
            in
                { model | writing = writing } ! [ Cmd.map (\m -> WritingMsg m) writingMsg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map (\m -> WritingMsg m) <| Writing.subscriptions model.writing
        ]


view : Model -> Html Msg
view editor =
    div []
        [ Html.map (\m -> WritingMsg m) <| Writing.view editor.writing
        , div [ style [ ( "white-space", "pre-wrap" ) ] ] [ text <| toJson editor.writing ]
        ]
