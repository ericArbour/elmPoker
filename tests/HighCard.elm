module HighCard exposing (..)

import CompareHands exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


highCardRank : Test
highCardRank =
    describe "weakest of each high card hand against the strongest of the card below it"
        [ test "weakest ace high beats strongest king high" <|
            \_ ->
                let
                    hand1 =
                        [ "AS"
                        , "2C"
                        , "3C"
                        , "4C"
                        , "6C"
                        ]

                    hand2 =
                        [ "KH"
                        , "8D"
                        , "TD"
                        , "JD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest king high beats strongest queen high" <|
            \_ ->
                let
                    hand1 =
                        [ "KS"
                        , "2C"
                        , "3C"
                        , "4C"
                        , "5C"
                        ]

                    hand2 =
                        [ "QH"
                        , "7D"
                        , "9D"
                        , "TD"
                        , "JH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest queen high beats strongest jack high" <|
            \_ ->
                let
                    hand1 =
                        [ "QS"
                        , "2C"
                        , "3C"
                        , "4C"
                        , "5C"
                        ]

                    hand2 =
                        [ "JH"
                        , "6D"
                        , "8D"
                        , "9D"
                        , "TH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest jack high beats strongest ten high" <|
            \_ ->
                let
                    hand1 =
                        [ "JS"
                        , "2C"
                        , "3C"
                        , "4C"
                        , "5C"
                        ]

                    hand2 =
                        [ "TH"
                        , "5D"
                        , "7D"
                        , "8D"
                        , "9H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest ten high beats strongest nine high" <|
            \_ ->
                let
                    hand1 =
                        [ "TS"
                        , "2C"
                        , "3C"
                        , "4C"
                        , "5C"
                        ]

                    hand2 =
                        [ "9H"
                        , "4D"
                        , "6D"
                        , "7D"
                        , "8H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest nine high beats strongest eight high" <|
            \_ ->
                let
                    hand1 =
                        [ "9S"
                        , "2C"
                        , "3C"
                        , "4C"
                        , "5C"
                        ]

                    hand2 =
                        [ "8H"
                        , "3D"
                        , "5D"
                        , "6D"
                        , "7H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest eight high beats strongest seven high" <|
            \_ ->
                let
                    hand1 =
                        [ "8S"
                        , "2C"
                        , "3C"
                        , "4C"
                        , "5C"
                        ]

                    hand2 =
                        [ "7H"
                        , "2D"
                        , "4D"
                        , "5D"
                        , "6H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        ]
