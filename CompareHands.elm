module CompareHands exposing (compareHands)

import Array exposing (..)
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


twoKVal : Dict String Int
twoKVal =
    Dict.fromList
        [ ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 5 ), ( "6", 8 ), ( "7", 13 ), ( "8", 21 ), ( "9", 34 ), ( "T", 55 ), ( "J", 89 ), ( "Q", 144 ), ( "K", 233 ), ( "A", 377 ) ]


oneKVal : Dict String Int
oneKVal =
    Dict.fromList
        [ ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 4 ), ( "6", 5 ), ( "7", 6 ), ( "8", 7 ), ( "9", 8 ), ( "T", 9 ), ( "J", 10 ), ( "Q", 11 ), ( "K", 12 ), ( "A", 13 ) ]


cardIndex : Dict String Int
cardIndex =
    Dict.fromList
        [ ( "2", 0 ), ( "3", 1 ), ( "4", 2 ), ( "5", 3 ), ( "6", 4 ), ( "7", 5 ), ( "8", 6 ), ( "9", 7 ), ( "T", 8 ), ( "J", 9 ), ( "Q", 10 ), ( "K", 11 ), ( "A", 12 ) ]


aceLowIndex : Dict String Int
aceLowIndex =
    Dict.fromList
        [ ( "A", 0 ), ( "2", 1 ), ( "3", 2 ), ( "4", 3 ), ( "5", 4 ), ( "6", 5 ), ( "7", 6 ), ( "8", 7 ), ( "9", 8 ), ( "T", 9 ), ( "J", 10 ), ( "Q", 11 ), ( "K", 12 ) ]


dictLookup : Dict String Int -> String -> Int
dictLookup dict face =
    let
        value =
            Dict.get face dict
    in
    case value of
        Just number ->
            number

        Nothing ->
            0


getFiveKVal : String -> Int
getFiveKVal =
    dictLookup fiveKVal


getThreeKVal : String -> Int
getThreeKVal =
    dictLookup threeKVal


getTwoKVal : String -> Int
getTwoKVal =
    dictLookup twoKVal


getOneKVal : String -> Int
getOneKVal =
    dictLookup oneKVal


getIndex : String -> Int
getIndex =
    dictLookup cardIndex


getAceLowIndex : String -> Int
getAceLowIndex =
    dictLookup aceLowIndex


getFace : String -> String
getFace card =
    String.left 1 card


getSuit : String -> String
getSuit card =
    String.right 1 card


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


countMatches : Int -> List ( String, Int ) -> Int
countMatches qty hand =
    List.foldl
        (\item accum ->
            if Tuple.second item == qty then
                accum + 1
            else
                accum
        )
        0
        hand


sortFaces : String -> String -> Order
sortFaces a b =
    compare (getIndex b) (getIndex a)


checkStraight : List String -> Maybe String
checkStraight hand =
    if List.length hand == 5 then
        let
            arrayHand =
                Array.fromList hand

            first : Maybe String
            first =
                Array.get 0 arrayHand

            last : Maybe String
            last =
                Array.get 4 arrayHand

            aceHighResult : Maybe String
            aceHighResult =
                case first of
                    Just f ->
                        case last of
                            Just l ->
                                if getIndex f - getIndex l == 4 then
                                    Just f
                                else
                                    Nothing

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing

            aceLowHand : Maybe (Array String)
            aceLowHand =
                case first of
                    Just f ->
                        if f == "A" then
                            Just (Array.push "A" (Array.slice 1 5 arrayHand))
                        else
                            Nothing

                    Nothing ->
                        Nothing

            aceLowFirst : Maybe String
            aceLowFirst =
                case aceLowHand of
                    Just array ->
                        Array.get 0 array

                    Nothing ->
                        Nothing

            aceLowLast : Maybe String
            aceLowLast =
                case aceLowHand of
                    Just array ->
                        Array.get 4 array

                    Nothing ->
                        Nothing

            aceLowResult : Maybe String
            aceLowResult =
                case aceLowFirst of
                    Just f ->
                        case aceLowLast of
                            Just l ->
                                if getAceLowIndex f - getAceLowIndex l == 4 then
                                    Just f
                                else
                                    Nothing

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
        in
        case aceHighResult of
            Just face ->
                Just face

            Nothing ->
                case aceLowResult of
                    Just face ->
                        Just face

                    Nothing ->
                        Nothing
    else
        Nothing


checkFlush : List String -> Bool
checkFlush hand =
    if
        List.length
            (List.foldr
                removeDups
                []
                (List.map getSuit hand)
            )
            == 1
    then
        True
    else
        False


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



-- High Card Rank : 1 - 2010 points. EQ: Sum card values.
-- One Pair : 2011 - 24858 points. EQ: 2002 + (PFi * 1826) + k
-- Two Pair: 24859 - 20109 points. EQ: 18226 + ((P1i - 1) * ((P1okv - 1) * 13)) + (P2okv * 13) + k
-- Set: 20110 - x points. EQ: constant + (Si * 608) + k
