module CompareHands exposing (compareHands)

import Dict exposing (..)
import Tuple exposing (..)


cardValue : Dict String Int
cardValue =
    Dict.fromList
        [ ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 4 ), ( "6", 7 ), ( "7", 12 ), ( "8", 21 ), ( "9", 38 ), ( "T", 75 ), ( "J", 141 ), ( "Q", 273 ), ( "K", 530 ), ( "A", 1028 ) ]


cardIndex : Dict String Int
cardIndex =
    Dict.fromList
        [ ( "2", 0 ), ( "3", 1 ), ( "4", 2 ), ( "5", 3 ), ( "6", 4 ), ( "7", 5 ), ( "8", 6 ), ( "9", 7 ), ( "T", 8 ), ( "J", 9 ), ( "Q", 10 ), ( "K", 11 ), ( "A", 12 ) ]


dictLookup : Dict String Int -> String -> Int
dictLookup dict face =
    let
        value =
            get face dict
    in
    case value of
        Just number ->
            number

        Nothing ->
            0


getValue =
    dictLookup cardValue


getIndex =
    dictLookup cardIndex


getFace : String -> String
getFace card =
    String.left 1 card


checkPairs : String -> List String -> List String
checkPairs face hand =
    hand


filterHand : List String -> String -> List String
filterHand hand face =
    List.filter (\card -> card == face) hand


removeDups : String -> List String -> List String
removeDups string list =
    if List.member string list then
        list
    else
        string :: list


calcHighCard : String -> Int -> ( Int, Int ) -> ( Int, Int )
calcHighCard face count accum =
    Tuple.mapFirst (\x -> getValue face + x) accum


calcOnePair : String -> Int -> ( Int, Int ) -> ( Int, Int )
calcOnePair face count accum =
    if count == 2 then
        Tuple.mapFirst (\x -> (getIndex face * 1826) + 2002 + x) accum
    else
        Tuple.mapFirst (\x -> getValue face + x) accum


calcTwoPair : String -> Int -> ( Int, Int ) -> ( Int, Int )
calcTwoPair face count accum =
    if count == 2 then
        if Tuple.second accum == 0 then
            let
                incrAccum =
                    Tuple.mapSecond (\x -> x + 1) accum
            in
            Tuple.mapFirst (\x -> (getIndex face * 1028) + x) incrAccum
        else
            Tuple.mapFirst (\x -> (getIndex face * 13364) + 24858 + x) accum
    else
        Tuple.mapFirst (\x -> getValue face + x) accum


countPairs : Dict String Int -> Int
countPairs hand =
    Dict.foldl
        (\face count accum ->
            if count == 2 then
                accum + 1
            else
                accum
        )
        0
        hand


transform : List String -> Int
transform hand =
    let
        faces =
            hand |> List.map getFace

        uniqueFaces =
            List.foldr
                removeDups
                []
                faces

        faceCount =
            uniqueFaces
                |> List.map (filterHand faces)
                |> List.map (\list -> List.length list)

        countLookup =
            Dict.fromList (List.map2 (,) uniqueFaces faceCount)

        pairCount =
            Debug.log "count lookup"
                (countPairs countLookup)

        value =
            if pairCount == 2 then
                Tuple.first (Dict.foldl calcTwoPair ( 0, 0 ) countLookup)
            else if pairCount == 1 then
                Tuple.first (Dict.foldl calcOnePair ( 0, 0 ) countLookup)
            else
                Tuple.first (Dict.foldl calcHighCard ( 0, 0 ) countLookup)
    in
    value


compareHands : List String -> List String -> Order
compareHands hand1 hand2 =
    let
        value1 =
            Debug.log "hand1"
                (transform hand1)

        value2 =
            Debug.log "hand2"
                (transform hand2)
    in
    if value1 > value2 then
        GT
    else if value2 > value1 then
        LT
    else
        EQ



-- High Card Rank : 1 - 2010 points. EQ: Sum card values.
-- One Pair : 2011 - 24858 points. EQ: 2002 + (PFi * 1826) + k
-- Two Pair: 24859 - x points. EQ: const + (PF1i * f1) + (PF2i * f2) + k
