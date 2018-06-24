module Straights exposing (..)

import CompareHands.CompareHands exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


straights : Test
straights =
    describe "straight against the strongest hand below it"
        [ test "weakest straight beats strongest set" <|
            \_ ->
                let
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
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "six high straight beats five high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3C"
                        , "4C"
                        , "5C"
                        , "6C"
                        ]

                    hand2 =
                        [ "AH"
                        , "2D"
                        , "3D"
                        , "4D"
                        , "5H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "seven high straight beats six high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "3S"
                        , "4C"
                        , "5C"
                        , "6C"
                        , "7C"
                        ]

                    hand2 =
                        [ "2H"
                        , "3D"
                        , "4D"
                        , "5D"
                        , "6H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "eight high straight beats seven high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "4S"
                        , "5C"
                        , "6C"
                        , "7C"
                        , "8C"
                        ]

                    hand2 =
                        [ "3H"
                        , "4D"
                        , "5D"
                        , "6D"
                        , "7H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "nine high straight beats eight high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "5S"
                        , "6C"
                        , "7C"
                        , "8C"
                        , "9C"
                        ]

                    hand2 =
                        [ "4H"
                        , "5D"
                        , "6D"
                        , "7D"
                        , "8H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "ten high straight beats nine high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "6S"
                        , "7C"
                        , "8C"
                        , "9C"
                        , "TC"
                        ]

                    hand2 =
                        [ "5H"
                        , "6D"
                        , "7D"
                        , "8D"
                        , "9H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "jack high straight beats ten high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "7S"
                        , "8C"
                        , "9C"
                        , "TC"
                        , "JC"
                        ]

                    hand2 =
                        [ "6H"
                        , "7D"
                        , "8D"
                        , "9D"
                        , "TH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "queen high straight beats jack high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "8S"
                        , "9C"
                        , "TC"
                        , "JC"
                        , "QC"
                        ]

                    hand2 =
                        [ "7H"
                        , "8D"
                        , "9D"
                        , "TD"
                        , "JH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "king high straight beats queen high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "9S"
                        , "TC"
                        , "JC"
                        , "QC"
                        , "KC"
                        ]

                    hand2 =
                        [ "8H"
                        , "9D"
                        , "TD"
                        , "JD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "ace high straight beats king high straight" <|
            \_ ->
                let
                    hand1 =
                        [ "TS"
                        , "JC"
                        , "QC"
                        , "KC"
                        , "AC"
                        ]

                    hand2 =
                        [ "9H"
                        , "TD"
                        , "JD"
                        , "QD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        ]
