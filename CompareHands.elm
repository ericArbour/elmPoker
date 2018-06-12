module CompareHands exposing (compareHands)

import Dict exposing (..)


cardRank : Dict String Int
cardRank =
    Dict.fromList
        [ ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 4 ), ( "6", 7 ), ( "7", 12 ), ( "8", 21 ), ( "9", 38 ), ( "T", 75 ), ( "J", 141 ), ( "Q", 273 ), ( "K", 530 ), ( "A", 1028 ) ]


getRank : String -> Int
getRank card =
    let
        value =
            get (String.left 1 card) cardRank
    in
    case value of
        Just number ->
            number

        Nothing ->
            0


compareHands : List String -> List String -> Order
compareHands hand1 hand2 =
    let
        hand1Value =
            Debug.log "hand1"
                (hand1 |> List.map getRank |> List.sum)

        hand2Value =
            Debug.log "hand2"
                (hand2 |> List.map getRank |> List.sum)
    in
    if hand1Value > hand2Value then
        GT
    else if hand2Value > hand1Value then
        LT
    else
        EQ
