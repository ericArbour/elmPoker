module OnePair exposing (..)

import CompareHands exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


onePair : Test
onePair =
    describe "weakest single pair hand against the strongest pair below it"
        [ test "weakest pair of twos beats highest high card hand" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "2C"
                        , "3C"
                        , "4C"
                        , "5C"
                        ]

                    hand2 =
                        [ "AH"
                        , "KD"
                        , "QD"
                        , "JD"
                        , "9H"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of threes beats strongest pair of twos" <|
            \_ ->
                let
                    hand1 =
                        [ "3S"
                        , "3C"
                        , "2C"
                        , "4C"
                        , "5C"
                        ]

                    hand2 =
                        [ "2H"
                        , "2D"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of fours beats strongest pair of threes" <|
            \_ ->
                let
                    hand1 =
                        [ "4S"
                        , "4C"
                        , "2C"
                        , "3C"
                        , "5C"
                        ]

                    hand2 =
                        [ "3H"
                        , "3D"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of fives beats strongest pair of fours" <|
            \_ ->
                let
                    hand1 =
                        [ "5S"
                        , "5C"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "4H"
                        , "4D"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of sixes beats strongest pair of fives" <|
            \_ ->
                let
                    hand1 =
                        [ "6S"
                        , "6C"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "5H"
                        , "5D"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of sevens beats strongest pair of sixes" <|
            \_ ->
                let
                    hand1 =
                        [ "7S"
                        , "7C"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "6H"
                        , "6D"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of eights beats strongest pair of sevens" <|
            \_ ->
                let
                    hand1 =
                        [ "8S"
                        , "8C"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "7H"
                        , "7D"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of nines beats strongest pair of eights" <|
            \_ ->
                let
                    hand1 =
                        [ "9S"
                        , "9C"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "8H"
                        , "8D"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of tens beats strongest pair of nines" <|
            \_ ->
                let
                    hand1 =
                        [ "TS"
                        , "TC"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "9H"
                        , "9D"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest jacks of threes beats strongest pair of tens" <|
            \_ ->
                let
                    hand1 =
                        [ "JS"
                        , "JC"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "TH"
                        , "TD"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest queens of threes beats strongest pair of jacks" <|
            \_ ->
                let
                    hand1 =
                        [ "QS"
                        , "QC"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "JH"
                        , "JD"
                        , "AD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of kings beats strongest pair of queens" <|
            \_ ->
                let
                    hand1 =
                        [ "KS"
                        , "KC"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "QH"
                        , "QD"
                        , "AD"
                        , "KD"
                        , "JH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest pair of aces beats strongest pair of kings" <|
            \_ ->
                let
                    hand1 =
                        [ "AS"
                        , "AC"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "KH"
                        , "KD"
                        , "AD"
                        , "QD"
                        , "JH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        ]
