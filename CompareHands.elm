module CompareHands exposing (compareHands)

import Dict exposing (..)
import Tuple exposing (..)


fiveKVal : Dict String Int
fiveKVal =
    Dict.fromList
        [ ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 4 ), ( "6", 5 ), ( "7", 6 ), ( "8", 10 ), ( "9", 18 ), ( "T", 33 ), ( "J", 62 ), ( "Q", 119 ), ( "K", 229 ), ( "A", 444 ) ]


threeKVal : Dict String Int
threeKVal =
    Dict.fromList
        [ ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 4 ), ( "6", 7 ), ( "7", 12 ), ( "8", 20 ), ( "9", 37 ), ( "T", 67 ), ( "J", 122 ), ( "Q", 224 ), ( "K", 411 ), ( "A", 755 ) ]


oneKVal : Dict String Int
oneKVal =
    Dict.fromList
        [ ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 4 ), ( "6", 5 ), ( "7", 6 ), ( "8", 7 ), ( "9", 8 ), ( "T", 9 ), ( "J", 10 ), ( "Q", 11 ), ( "K", 12 ), ( "A", 13 ) ]


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


getFiveKVal =
    dictLookup fiveKVal


getThreeKVal =
    dictLookup threeKVal


getOneKVal =
    dictLookup oneKVal


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


calcHighCard : ( String, Int ) -> ( Int, Int ) -> ( Int, Int )
calcHighCard item accum =
    Tuple.mapFirst (\x -> getFiveKVal (Tuple.first item) + x) accum


calcOnePair : ( String, Int ) -> ( Int, Int ) -> ( Int, Int )
calcOnePair item accum =
    if Tuple.second item == 2 then
        Tuple.mapFirst (\x -> (getIndex (Tuple.first item) * 1385) + 864 + x) accum
    else
        Tuple.mapFirst (\x -> getThreeKVal (Tuple.first item) + x) accum


calcTwoPair : ( String, Int ) -> ( Int, Int ) -> ( Int, Int )
calcTwoPair item accum =
    if Tuple.second item == 2 then
        if Tuple.second accum == 0 then
            let
                incrAccum =
                    Tuple.mapSecond (\x -> x + 1) accum
            in
            Tuple.mapFirst (\x -> (getIndex (Tuple.first item) - 1) * ((getOneKVal (Tuple.first item) - 1) * 13) + 18226 + x) incrAccum
        else
            Tuple.mapFirst (\x -> (getOneKVal (Tuple.first item) * 13) + x) accum
    else
        Tuple.mapFirst (\x -> getOneKVal (Tuple.first item) + x) accum


countPairs : List ( String, Int ) -> Int
countPairs hand =
    List.foldl
        (\item accum ->
            if Tuple.second item == 2 then
                accum + 1
            else
                accum
        )
        0
        hand


sortFaces : String -> String -> Order
sortFaces a b =
    compare (getIndex b) (getIndex a)


transform : List String -> Int
transform hand =
    let
        faces =
            hand |> List.map getFace

        orderedFaces =
            List.sortWith
                sortFaces
                faces

        uniqueFaces =
            List.foldr
                removeDups
                []
                orderedFaces

        faceCount =
            uniqueFaces
                |> List.map (filterHand orderedFaces)
                |> List.map (\list -> List.length list)

        countLookup =
            List.map2 (,) uniqueFaces faceCount

        pairCount =
            countPairs countLookup

        value =
            if pairCount == 2 then
                Tuple.first (List.foldl calcTwoPair ( 0, 0 ) countLookup)
            else if pairCount == 1 then
                Tuple.first (List.foldl calcOnePair ( 0, 0 ) countLookup)
            else
                Tuple.first (List.foldl calcHighCard ( 0, 0 ) countLookup)
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
-- Two Pair: 24859 - x points. EQ: 18226 + ((P1i - 1) * ((P1okv - 1) * 13)) + (P2okv * 13) + k
