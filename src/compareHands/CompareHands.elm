module CompareHands.CompareHands exposing (compareHands)

import CompareHands.Calculators exposing (..)
import CompareHands.Checkers exposing (..)
import CompareHands.Helpers exposing (..)
import Tuple exposing (..)


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
                        calcStraight highCard + 1201
                    else
                        calcStraight highCard

                Nothing ->
                    if quadCount == 1 then
                        Tuple.first (List.foldl calcQuads ( 0, 0 ) countLookup)
                    else if setCount == 1 && pairCount == 1 then
                        Tuple.first (List.foldl calcUncleJoey ( 0, 0 ) countLookup)
                    else if isFlush then
                        Tuple.first (List.foldl calcHighCard ( 0, 0 ) countLookup) + 27773
                    else if setCount == 1 then
                        Tuple.first (List.foldl calcSet ( 0, 0 ) countLookup)
                    else if pairCount == 2 then
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
