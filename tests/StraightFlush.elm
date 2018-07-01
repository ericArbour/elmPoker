module StraightFlush exposing (..)

import CompareHands.CompareHands exposing (justOrder)
import Expect exposing (Expectation)
import Test exposing (..)


straightFlush : Test
straightFlush =
    describe "straight flush against the strongest hand below it"
        [ test "weakest straight flush beats strongest four of a kind" <|
            \_ ->
                let
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
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "six high straight flush beats five high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2C"
                        , "3C"
                        , "4C"
                        , "5C"
                        , "6C"
                        ]

                    hand2 =
                        [ "AD"
                        , "2D"
                        , "3D"
                        , "4D"
                        , "5D"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "seven high straight flush beats six high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "3C"
                        , "4C"
                        , "5C"
                        , "6C"
                        , "7C"
                        ]

                    hand2 =
                        [ "2D"
                        , "3D"
                        , "4D"
                        , "5D"
                        , "6D"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "eight high straight flush beats seven high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "4C"
                        , "5C"
                        , "6C"
                        , "7C"
                        , "8C"
                        ]

                    hand2 =
                        [ "3D"
                        , "4D"
                        , "5D"
                        , "6D"
                        , "7D"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "nine high straight flush beats eight high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "5C"
                        , "6C"
                        , "7C"
                        , "8C"
                        , "9C"
                        ]

                    hand2 =
                        [ "4D"
                        , "5D"
                        , "6D"
                        , "7D"
                        , "8D"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "ten high straight flush beats nine high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "6C"
                        , "7C"
                        , "8C"
                        , "9C"
                        , "TC"
                        ]

                    hand2 =
                        [ "5D"
                        , "6D"
                        , "7D"
                        , "8D"
                        , "9D"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "jack high straight flush beats ten high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "7S"
                        , "8S"
                        , "9S"
                        , "TS"
                        , "JS"
                        ]

                    hand2 =
                        [ "6C"
                        , "7C"
                        , "8C"
                        , "9C"
                        , "TC"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "queen high straight flush beats jack high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "8S"
                        , "9S"
                        , "TS"
                        , "JS"
                        , "QS"
                        ]

                    hand2 =
                        [ "7H"
                        , "8H"
                        , "9H"
                        , "TH"
                        , "JH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "king high straight flush beats queen high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "9H"
                        , "TH"
                        , "JH"
                        , "QH"
                        , "KH"
                        ]

                    hand2 =
                        [ "8D"
                        , "9D"
                        , "TD"
                        , "JD"
                        , "QD"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "ace high straight flush beats king high straight flush" <|
            \_ ->
                let
                    hand1 =
                        [ "TC"
                        , "JC"
                        , "QC"
                        , "KC"
                        , "AC"
                        ]

                    hand2 =
                        [ "9H"
                        , "TH"
                        , "JH"
                        , "QH"
                        , "KH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        ]
