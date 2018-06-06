module HighCard exposing (highCard)

import Dict exposing (..)


cardRank : Dict String Int
cardRank =
    Dict.fromList
        [ ( "2", 0 ), ( "3", 1 ), ( "4", 2 ), ( "5", 3 ), ( "6", 4 ), ( "7", 5 ), ( "8", 6 ), ( "9", 7 ), ( "T", 8 ), ( "J", 9 ), ( "Q", 10 ), ( "K", 11 ), ( "A", 12 ) ]


compareRank : String -> String -> Maybe Order
compareRank card1 card2 =
    let
        card1Value =
            Dict.get card1 cardRank

        card2Value =
            Dict.get card2 cardRank
    in
    case card1Value of
        Nothing ->
            Nothing

        Just card1Value ->
            case card2Value of
                Nothing ->
                    Nothing

                Just card2Value ->
                    Just (compare card1Value card2Value)


highestOfTwoCards : String -> String -> String
highestOfTwoCards card1 card2 =
    let
        result =
            compareRank
                (String.left 1 card1)
                (String.left 1 card2)
    in
    case result of
        Nothing ->
            card1

        Just EQ ->
            card1

        Just GT ->
            card1

        Just LT ->
            card2


highCard : List String -> String
highCard cards =
    List.foldl highestOfTwoCards "2" cards
