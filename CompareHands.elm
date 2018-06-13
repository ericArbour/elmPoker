module CompareHands exposing (compareHands)

import Dict exposing (..)


cardRank : Dict String Int
cardRank =
    Dict.fromList
        [ ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 4 ), ( "6", 7 ), ( "7", 12 ), ( "8", 21 ), ( "9", 38 ), ( "T", 75 ), ( "J", 141 ), ( "Q", 273 ), ( "K", 530 ), ( "A", 1028 ) ]



-- getRank : String -> Int
-- getRank card =
--     let
--         value =
--             get (String.left 1 card) cardRank
--     in
--     case value of
--         Just number ->
--             number
--
--         Nothing ->
--             0


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


transform : List String -> Dict String Int
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
    in
    countLookup


compareHands : List String -> List String -> Order
compareHands hand1 hand2 =
    let
        countDim1 =
            Debug.log "hand1"
                (transform hand1)

        countDim2 =
            Debug.log "hand2"
                (transform hand2)
    in
    EQ



-- High Card Rank : 1 - 2010 points. EQ: Sum card values.
-- Pair : 2011 - x points.
