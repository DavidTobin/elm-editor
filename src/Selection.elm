module Selection exposing (Cursor(..))

import Block exposing (..)
import Element exposing (..)


type Cursor
    = Cursor Block Element Int
    | NoSelection
