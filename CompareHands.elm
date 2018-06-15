module CompareHands exposing (compareHands)

import Dict exposing (..)


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


calculate : String -> Int -> Int -> Int
calculate face count accum =
    if count == 2 then
        (getIndex face * 1826) + 2002 + accum
    else
        getValue face + accum


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

        value =
            Dict.foldl calculate 0 countLookup
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
-- Pair : 2011 - 23920 points. EQ: 2002 + (PFi * 1826) + k
