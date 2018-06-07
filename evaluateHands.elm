module EvaluateHands exposing (evaluateHands)

import Dict exposing (..)


cardRank : Dict String Int
cardRank =
    Dict.fromList
        [ ( "2", 0 ), ( "3", 1 ), ( "4", 2 ), ( "5", 3 ), ( "6", 4 ), ( "7", 5 ), ( "8", 6 ), ( "9", 7 ), ( "T", 8 ), ( "J", 9 ), ( "Q", 10 ), ( "K", 11 ), ( "A", 12 ) ]


compareRank : String -> String -> Order
compareRank card1 card2 =
    let
        card1Value =
            Dict.get (String.left 1 card1) cardRank

        card2Value =
            Dict.get (String.left 1 card2) cardRank
    in
    case card1Value of
        Nothing ->
            case card2Value of
                Nothing ->
                    Debug.log "compare error"
                        EQ

                Just card2Value ->
                    Debug.log "compare error"
                        LT

        Just card1Value ->
            case card2Value of
                Nothing ->
                    Debug.log "compare error"
                        GT

                Just card2Value ->
                    compare card1Value card2Value


highestOfTwoCards : String -> String -> String
highestOfTwoCards card1 card2 =
    let
        result =
            compareRank
                card1
                card2
    in
    case result of
        EQ ->
            card1

        GT ->
            card1

        LT ->
            card2


highCard : List String -> String
highCard cards =
    List.foldl highestOfTwoCards "2" cards


filterHelper : String -> String -> Bool
filterHelper check current =
    check /= current


evaluateHands : List String -> List String -> Order
evaluateHands hand1 hand2 =
    if List.length hand1 == 0 then
        EQ
    else
        let
            high1 =
                highCard hand1

            high2 =
                highCard hand2

            result =
                compareRank high1 high2
        in
        case result of
            GT ->
                GT

            LT ->
                LT

            EQ ->
                evaluateHands
                    (List.filter (filterHelper high1) hand1)
                    (List.filter (filterHelper high2) hand2)
