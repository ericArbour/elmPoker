module Main exposing (..)

import Array exposing (..)
import CompareHands exposing (compareHands)
import Html exposing (..)


straightOrder : Array String
straightOrder =
    Array.fromList [ "A", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A" ]


deck : List String
deck =
    [ "2H"
    , "3H"
    , "4H"
    , "5H"
    , "6H"
    , "7H"
    , "8H"
    , "9H"
    , "TH"
    , "JH"
    , "QH"
    , "KH"
    , "AH"
    , "2D"
    , "3D"
    , "4D"
    , "5D"
    , "6D"
    , "7D"
    , "8D"
    , "9D"
    , "TD"
    , "JD"
    , "QD"
    , "KD"
    , "AD"
    , "2S"
    , "3S"
    , "4S"
    , "5S"
    , "6S"
    , "7S"
    , "8S"
    , "9S"
    , "TS"
    , "JS"
    , "QS"
    , "KS"
    , "AS"
    , "2C"
    , "3C"
    , "4C"
    , "5C"
    , "6C"
    , "7C"
    , "8C"
    , "9C"
    , "TC"
    , "JC"
    , "QC"
    , "KC"
    , "AC"
    ]


hand1 =
    [ "3S"
    , "3C"
    , "2C"
    , "2C"
    , "4C"
    ]


hand2 =
    [ "AH"
    , "AD"
    , "KD"
    , "QD"
    , "JH"
    ]


test =
    compareHands hand1 hand2


main =
    div []
        [ text (toString test) ]
