module Main exposing (..)

import Array exposing (..)
import CompareHands exposing (compareHands)
import Html exposing (..)


straightOrder : Array String
straightOrder =
    Array.fromList [ "A", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A" ]


hand1 =
    [ "4S"
    , "4C"
    , "2C"
    , "2C"
    , "3C"
    ]


hand2 =
    [ "3H"
    , "3D"
    , "2D"
    , "2D"
    , "AH"
    ]


test =
    compareHands hand1 hand2


main =
    div []
        [ text (toString test) ]
