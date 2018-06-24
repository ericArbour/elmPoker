module CompareHands.GetValues exposing (getAceLowIndex, getFiveKVal, getIndex, getOneKVal, getThreeKVal, getTwoKVal)

import Dict exposing (..)


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
