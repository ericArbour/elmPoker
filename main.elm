module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


deck : List String
deck =
    [ "1H"
    , "2H"
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
    , "1D"
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
    , "1S"
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
    , "1C"
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


main =
    ul []
        (List.map
            (\card -> li [] [ text card ])
            deck
        )
