module Operations exposing (..)


appendText : Char -> Blocks -> Blocks
appendText char blocks =
    let
        lastBlock =
            List.tail blocks
    in
        lastBlock
            :: (blocks
                    |> List.take (List.length blocks - 1)
                    |> List.reverse
               )
