module Main exposing (..)

import Array exposing (..)
import CompareHands exposing (compareHands)
import Html exposing (..)


hand1 =
    [ "AS"
    , "2C"
    , "3C"
    , "4C"
    , "5C"
    ]


hand2 =
    [ "AH"
    , "AD"
    , "AD"
    , "KD"
    , "QH"
    ]


test =
    compareHands hand1 hand2


main =
    div []
        [ text (toString test) ]
