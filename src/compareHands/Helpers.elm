module CompareHands.Helpers exposing (..)

import CompareHands.GetValues exposing (getIndex)


getFace : String -> String
getFace card =
    String.left 1 card


getSuit : String -> String
getSuit card =
    String.right 1 card


filterHand : List String -> String -> List String
filterHand hand face =
    List.filter (\card -> card == face) hand


removeDups : String -> List String -> List String
removeDups string list =
    if List.member string list then
        list
    else
        string :: list


countMatches : Int -> List ( String, Int ) -> Int
countMatches qty hand =
    List.foldl
        (\item accum ->
            if Tuple.second item == qty then
                accum + 1
            else
                accum
        )
        0
        hand


sortFaces : String -> String -> Order
sortFaces a b =
    compare (getIndex b) (getIndex a)
