module Quads exposing (..)

import CompareHands.CompareHands exposing (justOrder)
import Expect exposing (Expectation)
import Test exposing (..)


quads : Test
quads =
    describe "weakest quads hand against the strongest below it"
        [ test "weakest quads beats strongest uncle joey" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "2C"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "AH"
                        , "AD"
                        , "AD"
                        , "KD"
                        , "KH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad threes beats strongest quad twos" <|
            \_ ->
                let
                    hand1 =
                        [ "3S"
                        , "3C"
                        , "3C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "2H"
                        , "2D"
                        , "2D"
                        , "2D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad fours beats strongest quad threes" <|
            \_ ->
                let
                    hand1 =
                        [ "4S"
                        , "4C"
                        , "4C"
                        , "4C"
                        , "2C"
                        ]

                    hand2 =
                        [ "3H"
                        , "3D"
                        , "3D"
                        , "3D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad fives beats strongest quad fours" <|
            \_ ->
                let
                    hand1 =
                        [ "5S"
                        , "5C"
                        , "5C"
                        , "5C"
                        , "2C"
                        ]

                    hand2 =
                        [ "4H"
                        , "4D"
                        , "4D"
                        , "4D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad sixes beats strongest quad fives" <|
            \_ ->
                let
                    hand1 =
                        [ "6S"
                        , "6C"
                        , "6C"
                        , "6C"
                        , "2C"
                        ]

                    hand2 =
                        [ "5H"
                        , "5D"
                        , "5D"
                        , "5D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad sevens beats strongest quad sixes" <|
            \_ ->
                let
                    hand1 =
                        [ "7S"
                        , "7C"
                        , "7C"
                        , "7C"
                        , "2C"
                        ]

                    hand2 =
                        [ "6H"
                        , "6D"
                        , "6D"
                        , "6D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad eights beats strongest quad sevens" <|
            \_ ->
                let
                    hand1 =
                        [ "8S"
                        , "8C"
                        , "8C"
                        , "8C"
                        , "2C"
                        ]

                    hand2 =
                        [ "7H"
                        , "7D"
                        , "7D"
                        , "7D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad nines beats strongest quad eights" <|
            \_ ->
                let
                    hand1 =
                        [ "9S"
                        , "9C"
                        , "9C"
                        , "9C"
                        , "2C"
                        ]

                    hand2 =
                        [ "8H"
                        , "8D"
                        , "8D"
                        , "8D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad tens beats strongest quad nines" <|
            \_ ->
                let
                    hand1 =
                        [ "TS"
                        , "TC"
                        , "TC"
                        , "TC"
                        , "2C"
                        ]

                    hand2 =
                        [ "9H"
                        , "9D"
                        , "9D"
                        , "9D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad jacks beats strongest quad tens" <|
            \_ ->
                let
                    hand1 =
                        [ "JS"
                        , "JC"
                        , "JC"
                        , "JC"
                        , "2C"
                        ]

                    hand2 =
                        [ "TH"
                        , "TD"
                        , "TD"
                        , "TD"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad queens beats strongest quad jacks" <|
            \_ ->
                let
                    hand1 =
                        [ "QS"
                        , "QC"
                        , "QC"
                        , "QC"
                        , "2C"
                        ]

                    hand2 =
                        [ "JH"
                        , "JD"
                        , "JD"
                        , "JD"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad kings beats strongest quad queens" <|
            \_ ->
                let
                    hand1 =
                        [ "KS"
                        , "KC"
                        , "KC"
                        , "KC"
                        , "2C"
                        ]

                    hand2 =
                        [ "QH"
                        , "QD"
                        , "QD"
                        , "QD"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest quad aces beats strongest quad kings" <|
            \_ ->
                let
                    hand1 =
                        [ "AS"
                        , "AC"
                        , "AC"
                        , "AC"
                        , "2C"
                        ]

                    hand2 =
                        [ "KH"
                        , "KD"
                        , "KD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        ]
