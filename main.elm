module Main exposing (..)

import Array exposing (..)
import CompareHands exposing (compareHands)
import Html exposing (..)


hand1 =
    [ "AC"
    , "2C"
    , "3C"
    , "4C"
    , "5C"
    ]


hand2 =
    [ "AH"
    , "AD"
    , "AD"
    , "AD"
    , "KH"
    ]


test =
    compareHands hand1 hand2


main =
    div []
        [ text (toString test) ]
