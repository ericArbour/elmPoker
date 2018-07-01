module CompareHands.CompareHands exposing (Result, compareHands, justOrder)

import CompareHands.Calculators exposing (..)
import CompareHands.Checkers exposing (..)
import CompareHands.Helpers exposing (..)
import Tuple exposing (..)


type alias Result =
    { order : Order
    , hand1 : String
    , hand2 : String
    }


transform : List String -> ( Int, String )
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
            countMatches 2 countLookup

        setCount =
            countMatches 3 countLookup

        quadCount =
            countMatches 4 countLookup

        isStraight =
            checkStraight uniqueFaces

        isFlush =
            checkFlush hand

        value =
            case isStraight of
                Just highCard ->
                    if isFlush then
                        ( calcStraight highCard + 1201, "Straight Flush" )
                    else
                        ( calcStraight highCard, "Straight" )

                Nothing ->
                    if quadCount == 1 then
                        ( Tuple.first (List.foldl calcQuads ( 0, 0 ) countLookup), "Quad" )
                    else if setCount == 1 && pairCount == 1 then
                        ( Tuple.first (List.foldl calcUncleJoey ( 0, 0 ) countLookup), "Uncle Joey" )
                    else if isFlush then
                        ( Tuple.first (List.foldl calcHighCard ( 0, 0 ) countLookup) + 27773, "Flush" )
                    else if setCount == 1 then
                        ( Tuple.first (List.foldl calcSet ( 0, 0 ) countLookup), "Set" )
                    else if pairCount == 2 then
                        ( Tuple.first (List.foldl calcTwoPair ( 0, 0 ) countLookup), "Two Pair" )
                    else if pairCount == 1 then
                        ( Tuple.first (List.foldl calcOnePair ( 0, 0 ) countLookup), "One Pair" )
                    else
                        ( Tuple.first (List.foldl calcHighCard ( 0, 0 ) countLookup), "High Card" )
    in
    value


compareHands : List String -> List String -> Result
compareHands hand1 hand2 =
    let
        result1 =
            transform hand1

        result2 =
            transform hand2
    in
    if Tuple.first result1 > Tuple.first result2 then
        { order = GT, hand1 = Tuple.second result1, hand2 = Tuple.second result2 }
    else if Tuple.first result2 > Tuple.first result1 then
        { order = LT, hand1 = Tuple.second result1, hand2 = Tuple.second result2 }
    else
        { order = EQ, hand1 = Tuple.second result1, hand2 = Tuple.second result2 }


justOrder : List String -> List String -> Order
justOrder hand1 hand2 =
    .order (compareHands hand1 hand2)
