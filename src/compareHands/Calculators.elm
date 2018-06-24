module CompareHands.Calculators exposing (..)

import CompareHands.GetValues exposing (getFiveKVal, getIndex, getOneKVal, getThreeKVal, getTwoKVal)


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
            Tuple.mapFirst (\x -> (getOneKVal (Tuple.first item) - 2) * ((getOneKVal (Tuple.first item) - 1) * 13) + 18226 + x) incrAccum
        else
            Tuple.mapFirst (\x -> (getOneKVal (Tuple.first item) * 13) + x) accum
    else
        Tuple.mapFirst (\x -> getOneKVal (Tuple.first item) + x) accum


calcSet : ( String, Int ) -> ( Int, Int ) -> ( Int, Int )
calcSet item accum =
    if Tuple.second item == 3 then
        Tuple.mapFirst (\x -> (getIndex (Tuple.first item) * 608) + 20105 + x) accum
    else
        Tuple.mapFirst (\x -> getTwoKVal (Tuple.first item) + x) accum


calcStraight : String -> Int
calcStraight highCard =
    getIndex highCard + 27776


calcUncleJoey : ( String, Int ) -> ( Int, Int ) -> ( Int, Int )
calcUncleJoey item accum =
    if Tuple.second item == 3 then
        Tuple.mapFirst (\x -> (getIndex (Tuple.first item) * 13) + 28644 + x) accum
    else if Tuple.second item == 2 then
        Tuple.mapFirst (\x -> getOneKVal (Tuple.first item) + x) accum
    else
        ( 0, 0 )


calcQuads : ( String, Int ) -> ( Int, Int ) -> ( Int, Int )
calcQuads item accum =
    if Tuple.second item == 4 then
        Tuple.mapFirst (\x -> (getIndex (Tuple.first item) * 13) + 28811 + x) accum
    else
        Tuple.mapFirst (\x -> getOneKVal (Tuple.first item) + x) accum
